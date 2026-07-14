---
title: Cached Bytecode Teardown Leaked Roots on Allocator Failure
date: 2026-07-14
category: runtime-errors
module: ZigTS cached bytecode ownership
problem_type: runtime_error
component: tooling
symptoms:
  - A Context teardown allocator failure returned normally while owned deserialized FunctionBytecode trees remained allocated.
  - Repeated cached-runtime teardown under allocator pressure could retain bytecode allocations instead of releasing them.
root_cause: memory_leak
resolution_type: code_fix
severity: medium
tags: [zigts, bytecode-cache, ownership, teardown, allocator-failure, memory-leak]
---

# Cached Bytecode Teardown Leaked Roots on Allocator Failure

## Problem

Cached bytecode is deserialized into a heap-owned function tree, verified, and transferred into `Context` before execution (`packages/runtime/src/zruntime.zig:1381`, `packages/runtime/src/zruntime.zig:1401`, `packages/runtime/src/zruntime.zig:1411`). Nested bytecode nodes can then also be reachable through runtime function objects: the context separately tracks function objects and deserialized roots, and its ownership comments identify cached function objects as borrowers (`packages/zigts/src/context.zig:313`, `packages/zigts/src/context.zig:317`).

The old teardown shape created an initially empty `FunctionBytecodeSeen` map during `Context.deinit` and populated it while destroying objects and roots. That made cleanup depend on a fresh allocation. `destroyFunctionBytecode` still shows the critical failure behavior: its `getOrPut` catches allocation failure and returns before freeing the function tree (`packages/zigts/src/object.zig:1529`). Under allocator pressure, a cached deserialized root could therefore be skipped and leaked.

## Symptoms

- Normal shutdown succeeded because the seen-set allocation usually succeeded.
- If allocations failed only after a real cached load, teardown could silently leave the cached function tree allocated. The destructor returns `void`, and its allocation failure path is an early return rather than an error reported to the caller (`packages/zigts/src/object.zig:1529`).
- Nested functions made ownership non-obvious because a function object and an ancestor constant pool can reach the same bytecode node; tracked destruction exists to prevent those paths from double-freeing it (`packages/zigts/src/object.zig:1494`).

## What Didn't Work

Building the seen-set during `deinit` was the failed design. A cleanup structure allocated only when cleanup begins cannot protect teardown from allocation failure; the first unseen node still requires `getOrPut`, whose failure abandons destruction (`packages/zigts/src/object.zig:1529`).

Blanket borrowing was also rejected. Treating every tracked function object as a borrower whenever any cached root exists would avoid a lookup, but cached and source-compiled bytecode can coexist in one runtime. The regression deliberately loads two cached roots and then source code in the same context (`packages/runtime/src/zruntime.zig:6043`). Source function objects retain their existing ownership path, where non-closure bytecode is destroyed through tracked destruction (`packages/zigts/src/object.zig:1589`).

An earlier broad review classified allocator-related `catch` forms as intentional best-effort handling without isolating this cached-bytecode lifecycle (session history). This bug needed a targeted ownership-transfer and teardown audit, not a repository-wide judgment about catch usage.

## Solution

`Context` now stores both the owned cached roots and a registry containing every bytecode node reachable from those roots (`packages/zigts/src/context.zig:317`). `takeBytecodeRoot` treats ownership transfer as a small transaction:

1. Ignore a root already owned by this context (`packages/zigts/src/context.zig:841`).
2. Recursively count unregistered nested bytecode nodes through bytecode constants (`packages/zigts/src/context.zig:855`).
3. Reserve capacity in both the root list and registry before changing ownership state (`packages/zigts/src/context.zig:848`).
4. Register the complete tree with `putAssumeCapacity`, then append the root with `appendAssumeCapacity` (`packages/zigts/src/context.zig:868`, `packages/zigts/src/context.zig:852`).

The runtime calls this transfer after cached bytecode verification and before executing the deserialized function (`packages/runtime/src/zruntime.zig:1401`, `packages/runtime/src/zruntime.zig:1411`). If reservation fails, `bytecode_transferred` remains false and the deserialization result retains cleanup responsibility (`packages/runtime/src/zruntime.zig:1388`).

During teardown, tracked function-object cleanup receives the pre-populated registry. Cached wrappers find their bytecode already present and therefore borrow it, while source-compiled function objects retain the existing tracked destruction behavior (`packages/zigts/src/context.zig:1065`). The context then destroys each cached root without a seen-set and deinitializes the ownership metadata (`packages/zigts/src/context.zig:1074`).

## Why This Works

All allocations needed to identify cached ownership happen before the context accepts the root. Once ownership transfers, every node in that cached tree is already registered, so wrapper cleanup does not need to grow the registry for cached nodes (`packages/zigts/src/context.zig:841`).

Each deserialization produces an independently owned tree. Duplicate transfer of the same root pointer is ignored, and the root list therefore contains one entry per owned tree (`packages/zigts/src/context.zig:841`, `packages/zigts/src/context.zig:1074`). Destroying those disjoint roots with no seen-set is safe because wrapper cleanup has already treated their registered nodes as borrowed (`packages/zigts/src/context.zig:1065`).

This guarantee is intentionally narrow: cached-bytecode ownership teardown is allocation-free after transfer. Source-compiled function teardown still uses the tracked seen-set path and is not claimed to be globally allocation-free (`packages/zigts/src/context.zig:1068`, `packages/zigts/src/object.zig:1529`).

## Prevention

- Ownership-transfer APIs should reserve all cleanup metadata before committing ownership. Do not defer ownership bookkeeping until destruction.
- Keep cached and source ownership explicit. A single runtime may contain both, so global mode flags or blanket borrowing rules are unsafe.
- Preserve the real-cache regression that arms `FailingAllocator` to reject every future allocation after load, then calls `deinit` (`packages/runtime/src/zruntime.zig:6027`). This proves the cached teardown path uses pre-reserved metadata.
- Preserve the mixed-ownership regression. Its two cached loads exercise two independently deserialized roots, and its subsequent source load verifies that cached and source bytecode remain separate owners (`packages/runtime/src/zruntime.zig:6043`).
- Verify changes with `zig build test-zigts test-zruntime -j1` and `bash scripts/verify.sh`.

## Related Issues

The same bytecode node may be encountered through a runtime function object and an enclosing function's constant pool. `FunctionBytecodeSeen` remains necessary for that general double-free defense (`packages/zigts/src/object.zig:1494`). This fix changes when cached ownership metadata is established; it does not replace the tracked destruction model for source-compiled bytecode.

Cached bytecode is verified before ownership transfer or execution (`packages/runtime/src/zruntime.zig:1401`). That ordering should remain intact: malformed input must not enter the ownership registry, and failed transfer must leave the deserialization result responsible for its own cleanup (`packages/runtime/src/zruntime.zig:1388`).
