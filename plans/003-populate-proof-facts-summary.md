# Populate Proof Facts Intent Summary

Planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`)
Planned on: 2026-06-20

## Drift Check

Before editing, verify the in-scope files have not changed since this plan was written:

```sh
git diff --name-only f2c637d -- packages/runtime/src/live_reload.zig packages/proof-review/src/review.zig packages/zigts/src/contract_types.zig
```

Expected output for direct execution from the planned commit is empty. If any path appears, re-open the changed file and re-check the field names before editing.

## Status

- Priority: P1
- Effort: M
- Risk: LOW
- Confidence: HIGH
- Category: proof / observability correctness
- Depends on: `plans/001-restore-format-baseline.md`

## Why

The live proof HUD and proof ledger claim to project one contract surface into `ReviewFacts`, but the current projection omits intent and behavior summary fields that already exist in the contract. That makes rendered and persisted proof facts less informative than the source contract.

## Current State

Evidence:

- `packages/proof-review/src/review.zig` defines `ReviewFacts.intent_assertion_count`, `intent_dynamic`, `behavior_path_count`, and `failure_path_count` at lines 256-260.
- `ReviewFacts.setIntentSummary` writes those fields at lines 332-347.
- `packages/zigts/src/contract_types.zig` defines `IntentInfo.assertions` and `IntentInfo.dynamic` at lines 1200-1207.
- `HandlerContract.intent` is available at lines 1296-1298.
- `HandlerContract.behaviors` is available at line 1334.
- Each `BehaviorPath` carries `is_failure_path` at lines 963-973.
- `packages/runtime/src/live_reload.zig` `factsFromContract` currently returns `ReviewFacts.fromProvenFacts(...)` directly at lines 830-836, with no call to `setIntentSummary`.

## Scope

In scope:

- `packages/runtime/src/live_reload.zig`
- A focused test in the same file or an existing proof review test file.

Out of scope:

- Changing proof verdict semantics.
- Treating intent assertions as proof obligations.
- Changing the serialized contract format.

## Steps

1. Add a small helper near `factsFromContract` that derives:

   - assertion count: `contract.intent.?.assertions.items.len`, or `0` when `intent` is null.
   - intent dynamic: `contract.intent.?.dynamic`, or `false` when `intent` is null.
   - behavior path count: `contract.behaviors.items.len`.
   - failure path count: count `behavior.is_failure_path`.

2. In `factsFromContract`, store the result of `ReviewFacts.fromProvenFacts(...)` in a local variable, call `facts.setIntentSummary(...)`, and return `facts`.

3. Add a regression test that builds a minimal `HandlerContract` with:

   - at least one intent assertion, or a dynamic intent with no assertions.
   - at least two behavior paths, one marked `is_failure_path = true`.

   Assert the resulting `ReviewFacts` fields match the contract.

4. Run:

   ```sh
   zig build test-zruntime test-proof-review --summary all
   zig build test --summary all
   git diff --check
   ```

## Done Criteria

- `factsFromContract` populates intent and behavior summary fields.
- The proof HUD and ledger continue to share the same projection path.
- Regression coverage exercises the new projection.
- Focused and aggregate tests pass.

## STOP Conditions

- Deriving failure path count requires data outside `HandlerContract.behaviors`.
- The test needs a broad contract-builder fixture unrelated to this projection.
- Any change would make intent assertions part of proof pass/fail semantics.
