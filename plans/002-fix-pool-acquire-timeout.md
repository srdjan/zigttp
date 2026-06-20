# Fix Pool Acquire Timeout Semantics

Planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`)
Planned on: 2026-06-20

## Drift Check

Before editing, verify the in-scope files have not changed since this plan was written:

```sh
git diff --name-only f2c637d -- packages/runtime/src/runtime_pool.zig packages/runtime/src/server_test.zig build.zig
```

Expected output for direct execution from the planned commit is empty. If any path appears, re-open the changed file and re-check the line evidence below.

## Status

- Priority: P1
- Effort: M
- Risk: MEDIUM
- Confidence: HIGH
- Category: runtime correctness
- Depends on: `plans/001-restore-format-baseline.md`

## Why

`acquire_timeout_ms` is an external runtime knob. The current implementation can return `PoolExhausted` because a retry counter trips, even when the configured timeout has not elapsed. Under contention, that makes admission control fail earlier than configured and can produce hard-to-debug request failures.

## Current State

Evidence from `packages/runtime/src/runtime_pool.zig`:

- `acquireForRequest` derives `timeout_ns` from `self.acquire_timeout_ms` at lines 512-515.
- The comment labels phase 3 as "Fail fast after 100 retries" at lines 516-520.
- `max_retries` is hard-coded to `100` at line 521.
- The code checks elapsed timeout at lines 561-569, then separately returns `error.PoolExhausted` when `retry_count > max_retries` at lines 572-579.

That means a nonzero timeout is not the only bound controlling the wait.

## Scope

In scope:

- `packages/runtime/src/runtime_pool.zig`
- A focused regression test near existing pool/runtime tests. Prefer `runtime_pool.zig`; use `server_test.zig` only if existing test harnesses make that simpler.

Out of scope:

- Changing request routing behavior.
- Changing default pool size, max size, or timeout configuration.
- Adding a broad benchmark suite.

## Steps

1. Remove `max_retries` as an independent failure condition for nonzero `acquire_timeout_ms`.

   Keep the existing spin/backoff shape:

   - timeout `0` should still fail immediately when capacity is exhausted.
   - nonzero timeout should fail only after elapsed time reaches `timeout_ns`.
   - successful acquisition before the timeout should work even after more than 100 backoff iterations.

2. Add a regression test for contended acquisition.

   Recommended shape:

   - create a pool with capacity `1` and a nonzero acquire timeout.
   - acquire or execute one runtime so the pool is at capacity.
   - arrange release before the configured timeout.
   - assert a second request/acquire succeeds instead of returning `PoolExhausted`.

   Avoid a pure wall-clock assertion if possible. If timing is unavoidable, use generous bounds and keep the test focused.

3. Confirm exhaustion still increments metrics and returns `PoolExhausted` after an actual timeout.

4. Run:

   ```sh
   zig build test-zruntime test-server --summary all
   zig build test --summary all
   git diff --check
   ```

## Done Criteria

- A nonzero acquire timeout is honored as the failure bound.
- Immediate fail-fast behavior for timeout `0` is preserved.
- Regression coverage fails on the planned-at implementation and passes after the change.
- Focused and aggregate tests pass.

## STOP Conditions

- A deterministic regression test requires a broad public API change.
- The proposed test flakes twice locally.
- The fix changes the meaning of `acquire_timeout_ms` beyond removing the hidden retry cap.
