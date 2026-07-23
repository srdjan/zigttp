# Plan 011: Force a recycle for `reuse_unbounded` runtimes before the atom table hits its hard cap

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only a4a731bd -- packages/runtime/src/runtime_pool.zig packages/runtime/src/contract_runtime.zig packages/zts/src/context.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW (adds a new bounded-recycle branch alongside three existing ones; does not change the other three policies' behavior)
- **Depends on**: none
- **Category**: bug (availability)
- **Planned at**: commit `a4a731bd` and 2026-07-02

## Why this matters

`PoolingPolicy.reuse_unbounded` is granted to handlers proven `pure && deterministic && state_isolated` — the framework's best-case, most-optimizable handler class (e.g. a JSON transform/echo handler). `runtime_pool.zig`'s recycle switch makes `policy_recycle` unconditionally `false` for this policy, so the underlying `Context`'s `AtomTable` and `HiddenClassPool` are never recycled by count or TTL, unlike the other three pooling policies which all have a bound. `AtomTable.intern` (`context.zig:1956-1981`) hard-fails with `error.OutOfMemory` once 65,534 distinct strings have been interned (a deliberate fail-closed guard against colliding with reserved hidden-class sentinels — see the comment at `context.zig:1964-1969` — not a bug in itself). A long-lived `reuse_unbounded` runtime that sees enough distinct property-key shapes over its process lifetime (e.g. `JSON.parse` on varied payloads, each distinct key set past the 64-entry/16-key `JSON_SHAPE_CACHE` interning a fresh atom) will eventually hit that cap and then fail `JSON.parse` and dynamic property access for every subsequent request on that runtime slot until process restart. This turns the framework's own reward for provably well-behaved handlers into a slow-building availability bug specifically for that class of handler.

## Current state

- `packages/runtime/src/contract_runtime.zig:49-66` — `PoolingPolicy` enum (`ephemeral`, `reuse_bounded_by_count`, `reuse_bounded_by_ttl`, `reuse_unbounded`) and `derivePoolingPolicy`, which returns `.reuse_unbounded` when `p.pure and p.deterministic and p.state_isolated`.
- `packages/runtime/src/contract_runtime.zig:56-58` — `PoolingThresholds`:
  ```zig
  pub const PoolingThresholds = struct {
      max_requests: u32 = 64,
      ttl_ns: u64 = 30 * std.time.ns_per_s,
  };
  ```
- `packages/runtime/src/runtime_pool.zig:676-719` — `ConnectionPool.releaseForRequest` (or equivalently-named method; confirm exact name at this line before editing — it is the function containing the `policy_recycle` switch). The switch:
  ```zig
  const policy_recycle = switch (self.pooling_policy) {
      .reuse_unbounded => false,
      .ephemeral => true,
      .reuse_bounded_by_count => request_count_after >= self.pooling_thresholds.max_requests,
      .reuse_bounded_by_ttl => blk: { ... },
  };
  ```
  `rt.user_data` is cast to `*Runtime` a few lines above (`runtime_pool.zig:678`), and `packages/runtime/src/zruntime.zig:158-160` confirms `Runtime` has a `ctx: *zq.Context` field, so `rt.ctx.atoms` is reachable from inside this function via the same cast already used for `runtime.prepareForPoolRelease()`.
- `packages/zts/src/context.zig:2023` — `pub fn count(self: *AtomTable) usize` already exists and returns the current interned-atom count (see `test "AtomTable count"` at `context.zig:2475`).
- `packages/zts/src/context.zig:1969` — the hard cap: `if (self.next_id >= 0xFFFE) return error.OutOfMemory;` (0xFFFE = 65,534).

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Build | `zig build` | exit 0 |
| zts tests (AtomTable) | `zig build test-zts` | success |
| ZRuntime tests (pooling policy) | `zig build test-zruntime` | success |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Full local gate | `bash scripts/verify.sh` | exit 0 |

## Suggested executor toolkit

- Existing `derivePoolingPolicy` tests in `contract_runtime.zig` (search `test "derivePoolingPolicy"`) show the established test style for this policy enum — model new threshold tests after them.
- Existing `reuse_bounded_by_count` recycle behavior is the direct analog for the new check; if there is a test asserting `reuse_bounded_by_count` recycles at `max_requests`, model the new `reuse_unbounded`-ceiling test after it.

## Scope

**In scope, the only files/directories to modify:**

- `packages/runtime/src/contract_runtime.zig` — add a new threshold field to `PoolingThresholds` (e.g. `max_dynamic_atoms: u32`) with a safe default well below the 65,534 hard cap.
- `packages/runtime/src/runtime_pool.zig` — add a `.reuse_unbounded` branch to the `policy_recycle` switch that forces a recycle once `rt.ctx.atoms.count() >= self.pooling_thresholds.max_dynamic_atoms`, using the existing `self.pool.dropRuntime(rt)` recycle mechanism already used by the other three policies (do not build a new in-place prune path — seem "Findings considered and rejected" below).

**Out of scope:**

- Wiring `AtomTable.pruneUnused`/`reset` into the release path as an in-place, non-recycling prune. This requires the caller to first compute a live-atom set (a GC-cooperation problem: which atoms are still referenced by live objects reachable from this runtime's globals/closures), which is unsafe to do casually mid-release and is a larger, separate design problem. This plan uses the simpler, already-proven recycle mechanism instead.
- `HiddenClassPool` eviction (mentioned in the original finding as a related unbounded structure). Recycling the whole `Context` on the new atom-count ceiling also discards its `HiddenClassPool`, so a separate eviction mechanism is not needed to close this specific availability gap.
- Changing the existing `reuse_bounded_by_count` / `reuse_bounded_by_ttl` / `ephemeral` branches.
- Changing `derivePoolingPolicy`'s classification logic (which handlers get which policy).

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(runtime): recycle reuse_unbounded runtimes before the atom table hits its hard cap`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Add the new threshold field

In `contract_runtime.zig`'s `PoolingThresholds`, add `max_dynamic_atoms: u32 = <default>`. Pick a default with real headroom below 0xFFFE (65,534) but high enough not to force needless recycles for legitimately shape-diverse handlers — e.g. 32,768 (half the hard cap) is a reasonable starting point; state your chosen default and rationale in the commit message.

**Verify**: `zig build test-zts` -> success (no behavior change yet, just a new field with a default).

### Step 2: Add the recycle branch

In `runtime_pool.zig`'s `policy_recycle` switch, change the `.reuse_unbounded` arm from the literal `false` to a count check against the new threshold, reading the atom count via the `Runtime` pointer already available in that function (see Current state). Keep the rest of the function (audit-poisoned handling, `self.recycles.fetchAdd`, `self.pool.dropRuntime(rt)`) unchanged — the new condition should simply feed into the existing `if (audit_poisoned or policy_recycle)` branch.

**Verify**: `zig build test-zruntime` -> success.

### Step 3: Add regression tests

Add a test that drives a `reuse_unbounded`-policy runtime through enough distinct atom interns to cross the new threshold and asserts it gets recycled (`self.recycles` counter increments, or the pool drops and reissues a fresh runtime on next acquire) rather than continuing to accumulate. Model the harness after existing pooling-policy tests in `contract_runtime.zig`/`runtime_pool.zig`. Also add (or confirm existing coverage for) a test that a `reuse_unbounded` runtime *under* the threshold is *not* recycled, to guard against a fix that recycles too eagerly.

**Verify**: `zig build test-zruntime test-zts` -> success; the "recycles once over threshold" test fails against the pre-fix `.reuse_unbounded => false` code (temporarily revert Step 2 locally to confirm, then reapply) — do not skip this red-proof.

## Test plan

- New test(s) in `runtime_pool.zig` (or `contract_runtime.zig` if the threshold check is more naturally unit-tested there in isolation): over-threshold triggers recycle; under-threshold does not.
- Regression: the over-threshold test must fail on the pre-fix code and pass after (Step 3 red-proof).
- Edge case: verify the check uses `>=` consistently with the existing `reuse_bounded_by_count` comparison style (`request_count_after >= self.pooling_thresholds.max_requests`).
- Final: `bash scripts/verify.sh` -> exit 0.

## Done criteria

All must hold:

- [ ] `reuse_unbounded` runtimes are recycled once their interned dynamic-atom count reaches the new configurable ceiling, using the existing `dropRuntime` mechanism.
- [ ] The other three pooling policies are unchanged.
- [ ] `zig build test-zruntime test-zts` exit 0.
- [ ] `bash scripts/verify.sh` exit 0.
- [ ] New tests cover both over-threshold (recycles) and under-threshold (does not recycle) cases, with the over-threshold case proven to fail pre-fix.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match the live code (re-check line numbers first).
- `rt.ctx.atoms` is not reachable from inside the `policy_recycle` switch's enclosing function the way the excerpt above assumes (e.g. if `rt.user_data` is null at this point, or the cast pattern has changed) — report the actual shape instead of forcing a workaround.
- A step's verification fails twice after reasonable local correction.
- You find that recycling mid-flight would drop live request state for `reuse_unbounded` handlers in a way the existing `reuse_bounded_by_count` recycle does not already handle safely — if `dropRuntime` on an in-use runtime is unsafe for `reuse_unbounded` specifically, stop and report rather than guessing at synchronization.

## Maintenance notes

- The chosen default for `max_dynamic_atoms` is a policy knob, not a hard correctness bound (unlike the 0xFFFE hard cap in `AtomTable.intern`, which must never be relaxed without also revisiting the hidden-class transition sentinel comment at `context.zig:1964-1969`). A future change may want to make it configurable per-deployment rather than a fixed default; that is a reasonable follow-up, not required here.
- If a future pass wants the in-place prune (`AtomTable.pruneUnused`/`reset`) instead of a full recycle, it needs a design for computing the live-atom set safely between requests — flagged here as deliberately deferred, not forgotten.
