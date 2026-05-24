# FailingAllocator Coverage

State of `std.testing.FailingAllocator` coverage across `packages/`. The pattern was introduced in commit `b8f0bbb` (precompile errdefer ladder fix) and extended once more under the structural-P0 slice.

## Pattern

Each test:
1. Builds minimal input that exercises the function's allocation sequence.
2. Runs once with `FailingAllocator.fail_index = maxInt(usize)` to find the ceiling allocation count.
3. Loops `fail_at` from 0 to ceiling, invoking the function with a `FailingAllocator` that fails at index `fail_at`.
4. On success, deinit. On error, asserts `error.OutOfMemory`.
5. The outer `testing.allocator` catches any leak on the unwind path.

The ceiling-then-walk shape catches errdefer mistakes that span multiple allocation points - it surfaces *every* index where rollback misses something.

## Sites covered

| Site | Test | Bugs found |
|------|------|------------|
| `packages/zigts/src/pool.zig` - `Runtime.create` | `Runtime.create errdefer ladder closes every failure path` | original |
| `packages/tools/src/precompile.zig` - `buildServiceTypeContextFromContracts` | same name in precompile.zig | original (b8f0bbb) |
| `packages/runtime/src/contract_runtime.zig` - `parseContractJson` | `parseContractJson: errdefer ladders close every failure path` | 5 leak sites |

## What `parseContractJson` uncovered

The new coverage caught a recurring `try X.append(allocator, try allocator.dupe(...))` anti-pattern:

```zig
try env_vars.append(allocator, try allocator.dupe(u8, item.string));
```

If `allocator.dupe` succeeds and `env_vars.append` fails, the duped string is orphaned: it is not yet in `env_vars.items`, so the outer cleanup walks past it. Fix:

```zig
const duped = try allocator.dupe(u8, item.string);
errdefer allocator.free(duped);
try env_vars.append(allocator, duped);
```

The errdefer on `duped` is scoped to the loop iteration. On a failing append, it fires and frees the orphan. On a successful append, ownership transfers to `env_vars`, the scope exits, and the iteration-scoped errdefer goes away - the outer errdefer on `env_vars.items` is the new owner.

Five sites fixed across `parseContractJson` (lines 284, 319, 365) and `fromHandlerContract` (lines 517, 530, 552). Three different shapes: single-field append, two-field route literal, and module-list append.

## Deferred targets

- **`HandlerPool.init`** (`runtime_pool.zig`). Calls `LockFreePool.init`, `BytecodeCache.init`, optional trace-file plumbing, and `prewarm()`. Driving it under `FailingAllocator` requires faking enough of the embedded-handler bytecode path that the test setup dominates the test value. The `errdefer self.deinit()` at line 180 is the safety net; a targeted test of just that ladder is feasible but lower leverage than what was found in `parseContractJson`.
- **`ContractBuilder`** (`contract_builder.zig`). Streaming accumulator used during parse; not a single-call surface. Would need a parser harness to drive realistic allocation sequences.

Both remain valuable but did not land in this slice. The same pattern applies when they do.
