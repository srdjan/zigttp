---
title: Builtin Graph Teardown Dereferenced Freed Prototypes
date: 2026-07-16
category: runtime-errors
module: ZigTS builtin object teardown
problem_type: runtime_error
component: tooling
symptoms:
  - Linux glibc tests aborted with double-free and fastbin-corruption diagnostics during Context teardown.
  - Guard Malloc faulted in JSValue.isObject while Context.deinit destroyed registered builtins.
  - Plain macOS runs occasionally crashed on a 0xaaaaaaaaaaaaaaae pointer during the same teardown loop.
root_cause: logic_error
resolution_type: code_fix
severity: high
tags: [zigts, builtins, teardown, use-after-free, ownership, guard-malloc, glibc]
---

# Builtin Graph Teardown Dereferenced Freed Prototypes

## Problem

`Context.builtin_objects` was destroyed as a flat owning list even though the
objects form a graph. Nine constructor `.prototype` properties pointed to roots
that the same teardown freed earlier, so the later `destroyBuiltin` traversal
read a freed object's header through `JSValue.isObject`.

The defect ran during every builtin teardown. Linux glibc made it visible by
overwriting the freed header with allocator metadata, while macOS allocators
usually left the stale read mapped and apparently harmless.

## Symptoms

- Guard Malloc deterministically faulted at `packages/zigts/src/value.zig` in
  `JSValue.isObject`, called from `Context.deinit`.
- glibc reported `double free or corruption (fasttop)` or
  `malloc_consolidate(): unaligned fastbin chunk` after a stale tag was
  misclassified as a string and freed again.
- Gating the named tests only moved the corruption into adjacent
  `HandlerPool` cases because the bug was in shared context teardown.

## What Didn't Work

- `DebugAllocator` and retained-memory modes could not detect the defect. The
  invalid operation was a raw pointer read, not an allocator API call, and
  keeping freed pages mapped made the read more likely to succeed.
- Skipping Linux tests hid the allocator symptom without changing the invalid
  lifetime. Twenty-seven gates across the runtime suites covered the same root
  cause.
- A first teardown split cleared function slot 0 before `destroyFull` ran.
  That slot owns `NativeFunctionData`, so the leak-checking suite reported
  thousands of leaks. Function metadata must be released during the scrub,
  before reserved slots become unreachable.
- A debug-only `.free` tag assertion in `isObject` and `isString` was not a
  valid general guard. ZigTS deliberately accepts raw pointer-like values whose
  first word can be zero, so the assertion rejected a supported truthiness test.

## Solution

Builtin cleanup is now two-phase and allocation-free.

`packages/zigts/src/object.zig` separates function metadata destruction from
object destruction. `scrubBuiltin` first releases the root's function metadata,
then walks its property slots. Each slot is cleared to `undefined` before its
old value is inspected. Untracked method functions are recursively scrubbed and
freed, and owned non-unique strings are freed exactly as before. The standalone
`destroyBuiltin` API remains as a `scrubBuiltin` plus object-free wrapper for OOM
`errdefer` paths.

`packages/zigts/src/context.zig` applies that operation in two passes:

1. Scrub all six prototype roots and every entry in `builtin_objects`.
2. Free the prototype and builtin root objects only after every tracked slot is
   pointer-free.

The global object is then freed with the same pass-two primitive. Its function
properties are owned by `builtin_objects`, so the old stale-reference
`global_obj` teardown workaround is no longer needed.

Once Guard Malloc completed cleanly, the Linux-only gates were removed from
`packages/runtime/src/zruntime.zig`, `packages/runtime/src/server_test.zig`,
`packages/runtime/src/ratchet_command.zig`, and
`packages/runtime/src/runtime_pool.zig`.

## Why This Works

No root object is freed while another tracked root can still contain a pointer
to it. The entire alias graph remains live during the scrub pass, so
`isObject` and `isString` only inspect allocated values. After the scrub, the
free pass reads object allocation metadata but never traverses property values.

The split does not allocate and does not depend on registration order. Moving
function metadata cleanup into the scrub preserves ownership even though the
reserved slots are cleared, while leak-checking tests prove that untracked
methods and strings were not dropped.

## Prevention

- Treat teardown registries as ownership graphs when stored values can point to
  other registered owners. Either remove edges before freeing nodes or carry
  explicit ownership metadata established before teardown.
- Run the affected suites one target at a time:

  ```bash
  zig build test-zruntime --summary all
  zig build test-server --summary all
  ```

- Use Guard Malloc for this class of raw-pointer lifetime defect; allocator leak
  detectors are not substitutes:

  ```bash
  DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib <test-binary>
  ```

- Keep the runtime leak tests strict. A teardown scrub that clears reserved
  ownership slots before releasing their metadata should fail loudly.
- Do not add a `.free` tag assertion to NaN-boxing accessors until the value
  representation can distinguish freed heap blocks from supported raw pointers.

## Related Issues

- [Cached Bytecode Teardown Leaked Roots on Allocator Failure](cached-bytecode-teardown-allocator-failure-leak.md)
  covers the same allocation-free teardown discipline at a different ownership
  boundary.
