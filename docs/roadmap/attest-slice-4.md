# Technical Spec: Proof Receipts, Slice 4 - Property Ratchet

Status: shipping in trickle. Initial slice landed 2026-05-19.
Predecessors: [attest-slice-1.md](attest-slice-1.md), [attest-slice-2.md](attest-slice-2.md), [attest-slice-3-kickoff.md](attest-slice-3-kickoff.md).
Slice scope: extend the proof receipt program with a *temporal* dimension. Slices 1-3 sign a per-build proof; slice 4 gates the next build on the previous one.

## Product Context

The compiler already infers and signs every handler's proven-property set:
`computeProperties()` in `contract_builder.zig` runs unconditionally, all 16
properties (pure, read_only, deterministic, injection_safe, ...) ride in
contract.json, and contract.json rides inside the signed JWS payload.

What was missing was the cross-build comparison. Yesterday's
`injection_safe` handler could silently lose that property today and no one
would notice until production. The autoloop closes the loop *inside* a
session; nothing closed it *between* sessions.

Slice 4 fixes that without inventing a new trust layer. It exposes the
proven set as a first-class JSON field, ships a CLI subcommand that diffs
two contracts and exits non-zero on regression, and surfaces the same
information through the expert.

After slice 4:

- contract.json carries a top-level `provenSpecs` array - the canonical
  list of property names the build proves true. Order is stable across
  builds (struct-field declaration order); the JWS payload covers it
  automatically because the JWS is computed over the contract bytes.
- `zigttp ratchet show <handler.ts>` compiles the handler and prints the
  current proven set. `zigttp ratchet check <handler.ts>` compiles the
  current handler, reads the `Spec<...>` obligations declared on the
  return type as the baseline, and exits 1 if any declared spec is not
  proven. The hand-maintained `--baseline <old.json>` file from the
  initial slice has been retired — the obligation set now lives in
  source next to the handler that owes it. Handlers that declare no
  `Spec<...>` exit clean.
- `zigts_expert_ratchet` is a new compiler-grounded tool that lets the
  expert read the current set programmatically before proposing edits.
- Existing `Spec<...>` author declarations are unchanged; the ratchet is
  additive. Authors who pin specific obligations still get ZTS500 when the
  obligation is broken. The ratchet adds a parallel rail for the
  *inferred* set so the entire property surface is monotonic by default.

Success metric for the initial slice: a handler that declares
`Spec<"injection_safe">` on its return type but no longer calls
`validateJson` causes `zigttp ratchet check` to exit 1 with
`injection_safe` named under "unmet", even though the same edit produces
a binary that runs and still signs a JWS.

## Architecture Decisions

### 1. provenSpecs as a redundant top-level field, derived from properties

Chosen. Reasons: the `properties` object inside contract.json is the
canonical bool-per-property representation; cross-build consumers want a
sorted list of names so set differences are mechanical. Emitting both
keeps backwards compatibility for the existing object readers while giving
ratchet, the expert, and the future well-known doc a directly diffable
shape.

Alternative considered: replace the object with the array. Rejected because
existing consumers (the deploy UI, the proof card, contract.json parsers
in tests) depend on the object shape.

### 2. CLI subcommand under `zigttp ratchet`, not `zigts prove`

Chosen. `zigts prove old.json new.json` is the generic contract diff that
classifies changes as safe/breaking/equivalent. `zigttp ratchet check` is a
*policy* command: it cares only about property regression, ignores other
contract drift, and is named for the human operation ("check the
ratchet") that the operator wants to run in CI. Two different audiences,
two different surfaces.

### 3. No signed waivers in the initial slice

Chosen for v1. Reasons: the waiver file format (signing key, file layout,
verification path on the CI side, rotation semantics, signed-after-the-fact
attack surface) deserves its own design conversation. The minimal landing
puts the ratchet in place with the strict-default behavior: any regression
fails. Operators who genuinely want to accept a regression today can pass
a stale baseline file; in slice 4.1 they will sign a waiver instead.

Alternative considered: ship unsigned waivers (a TOML file under
`.zigttp/waivers/` with a `reason` field). Rejected because unsigned
waivers add a trust hole without solving the cross-machine verification
story the rest of the attest program holds itself to.

### 4. Expert tool surfaces the proven set; /tighten and ratchet delta footer deferred

Chosen. `zigts_expert_ratchet` reports the current set. The richer expert
behaviors from the original plan (`/tighten <property>` that synthesizes
edits to unlock a named property, and a ratchet-delta footer on every edit
proposal in the existing veto loop) depend on the autoloop's existing
repair-plan machinery. Each is a small follow-up; the tool ships now so
manual workflows can use it immediately.

## Implementation Plan

What landed in this slice:

- `packages/zigts/src/contract_types.zig`
  - `HandlerProperties.provenSpecNames(out_buf) -> usize`: comptime-iterates
    boolean fields and fills `out_buf` with the names of those proven true.
    Returns the count; caller-side buffer must be at least
    `max_proven_specs` long.
  - `HandlerProperties.max_proven_specs: usize`: compile-time count of
    boolean fields, equal to the max size of any proven set.
- `packages/zigts/src/contract_json_writer.zig`
  - Emits `"provenSpecs": [...]` as a top-level field alongside the existing
    `properties` object. Empty array when properties are absent.
- `packages/zigts/src/contract_json_parser.zig`
  - No change required; unknown fields are already skipped, and the array
    is derivable from `properties`, so round-trip is preserved.
- `packages/runtime/src/ratchet_command.zig`
  - Implements `zigttp ratchet show <handler.ts>` and `zigttp ratchet
    check <handler.ts>`. The check compiles once, reads the proven set
    from `HandlerProperties.provenSpecNames` and the declared set from
    `contract.declared_specs` (populated from `Spec<...>` on the
    handler's return type), and exits 1 if any declared spec is not
    proven. The `--baseline <old.json>` flag from the initial slice has
    been retired; invoking it now prints a migration message and exits 1.
- `packages/runtime/src/dev_cli.zig`
  - Dispatches `zigttp ratchet ...` to the new module.
- `packages/pi/src/tools/zigts_expert_ratchet.zig` (new)
  - Tool that lets the expert read the current proven set programmatically.
- `packages/pi/src/app.zig`
  - Registers the new tool in `buildRegistry()`.

What did not land in this slice and is deferred:

- `Spec<"infer">` sugar. The plain `Spec<"X" | "Y">` form already covers
  the author-pin use case; the inference happens regardless of declaration.
  A typed-language pin is convenience over correctness; tracked as 4.1.
- Signed waivers (file format + signing + verification). Tracked as 4.2.
- Per-route ratchet object in the well-known doc. Tracked as 4.3; needs
  multi-route contracts before it has more than one entry.
- `/tighten <route> <property>` and the ratchet delta footer in the
  expert's veto loop. Tracked as 4.4.
- Autoloop `--goal infer-all`. Tracked as 4.5.

## Validation

Demo flow that proves the slice:

1. Write a handler that declares `Spec<"injection_safe">` on its return
   type and uses `validateJson` so the property holds. Run
   `zigttp ratchet show <handler.ts>` and confirm `injection_safe`
   appears in the proven set.
2. Run `zigttp ratchet check <handler.ts>` and confirm exit 0 with
   "ratchet held".
3. Edit the handler to drop the `validateJson` call but leave
   `Spec<"injection_safe">` on the return type. Re-run
   `zigttp ratchet check <handler.ts>`. Confirm exit 1 with
   `injection_safe` named under "unmet (declared but not proven)".
4. Restore `validateJson`. Re-run check. Confirm exit 0 and "ratchet held".

Regression guard: invoking `zigttp ratchet check --baseline anything.json
<handler.ts>` (any form: bare flag, `--baseline=anything.json`, typo'd
`--basline`, an unknown flag, or a second positional path) must exit 1
with a loud error. Silent acceptance of stale invocations would re-open
the very gap this slice exists to close.

Out-of-scope demo (slice 4.2):

- Generate a waiver file for the regression. Re-run check with the waiver
  present and confirm exit 0 even with `injection_safe` weakened.
