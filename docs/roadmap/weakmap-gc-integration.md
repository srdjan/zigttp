# WeakMap / WeakSet full GC integration (deferred to v0.2.0)

## Why this is deferred

`packages/zigts/src/builtins/weak_collections.zig` keys WeakMap and WeakSet entries by raw `JSObject` pointer addresses through `value.JSValue.getPtrAddress()`. The v0.1.0 implementation relies on two safety properties that hold today but are not formally enforced:

1. **Non-moving collector.** `packages/zigts/src/gc.zig` is mark-sweep with no compaction (`TenuredHeap.simdSweep` / `scalarSweep`). Pointers stay stable across collections, so the address used as a hash key remains valid for as long as the key object is reachable.
2. **Per-request arena tie-in.** When a `WeakMapData` is allocated inside a request (`ctx.hybrid` is non-null at `weak_collections.zig:60-67`), both the struct and its `entries` backing hash map live on the request arena and are reclaimed in one shot when `context.HybridAllocator.arena.reset()` runs at the end of the request.

Together these give a usable "weak map valid for the duration of one request" semantic. The pre-1.0 hardening pass added doc comments stating this contract and a regression test for the arena drop behavior (`weak_collections.zig` `test "WeakMapData arena tie-in drops entries on arena reset"`).

What is not covered:

- **Stale-pointer collision.** If a tenured key object is collected mid-request (or a non-arena-tied WeakMap survives across requests) and a new object is later allocated at the same address, `wm.get(newObj)` returns the dead value associated with the old key. The window is narrow because the only intra-request GC trigger today is allocator pressure, but the failure mode is real.
- **Stale-entry leak.** Entries are never pruned when the key dies. Long-lived non-arena WeakMaps grow monotonically with dead pointer keys.
- **Cross-request semantics.** A WeakMap constructed at runtime startup (e.g. as a global cache) lives on `ctx.allocator`, not the arena, and never sees a reset.

## What needs to change

The right fix wires WeakMap / WeakSet into the existing GC sweep path. The infrastructure is already in place: `TenuredHeap.simdSweep` and `scalarSweep` already accept a `free_callback: ?*const fn (*anyopaque) void` (`gc.zig:176`, `:252`, `:272`) that fires just before each unmarked object is freed (`gc.zig:228-231`). No caller supplies one today. The work:

1. **Class-dispatched finalizer.** Introduce a class-id-keyed dispatch table that the sweep callback consults. For `class_id == .weak_map` and `class_id == .weak_set`, fire a finalizer that deinits the embedded `WeakMapData` / `WeakSetData`. This closes the WeakMapData leak that exists today for non-arena allocations.
2. **Reverse index from key to map.** When `WeakMap.set(key, val)` runs, register the key's pointer address in a per-context `weak_key_registry: AutoHashMap(usize, ArrayList(*WeakMapData))`. On sweep, when a key object is about to be freed, look it up in the registry and call `data.delete(ptr)` for each map that referenced it. This closes the stale-pointer hole.
3. **Sweep-callback wiring.** Set the `free_callback` on `TenuredHeap` at GC init to a function that performs steps 1 and 2 in order. The nursery path (`NurseryHeap` bump allocation) needs its own hook; today nursery objects die at minor-GC time without notifying the registry.
4. **Class-id finalizer registry.** A small `pub const ClassFinalizer = fn (*anyopaque) void` table indexed by `class_id`. WeakMap, WeakSet, and any future class with non-trivial cleanup (sqlite stmts come to mind - currently handled at `module_binding.zig:908`) register here.

The aggregate cost is modest: the registry hash maps add one pointer per WeakMap entry, the sweep callback fires only for objects whose class has a registered finalizer, and the existing `getIndex(ptr) ?usize` accessor on `TenuredHeap` (`gc.zig:160-162`) gives the right structural query.

## Validation

After the change lands, the pre-1.0 regression test `test "WeakMapData arena tie-in drops entries on arena reset"` must still pass unchanged. Three new tests should join it:

- Allocate a tenured object, store it as a WeakMap key, run a major GC, allocate a new object at the (eventually) same address, assert `wm.get(newObj) == undefined`.
- Allocate a long-lived WeakMap on `ctx.allocator` (non-arena), populate, force GC, assert entry count drops.
- Allocate a WeakSet across two requests, assert no leak under `testing.allocator`.

## Related

- `docs/roadmap/zruntime-split.md` - the larger v0.2.0 split that absorbs adjacent runtime work.
- `packages/zigts/src/gc.zig:228-231` - the existing finalizer-callback hook that is the integration point.
- `packages/zigts/src/builtins/weak_collections.zig` doc comments on `WeakMapData` and `WeakSetData` - the user-facing contract that names this roadmap entry directly.
