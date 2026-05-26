# zruntime + interpreter monolith splits (deferred to v0.2.0)

## Why this is deferred

`packages/runtime/src/zruntime.zig` is 7,048 LOC and `packages/zigts/src/interpreter.zig` is 5,398 LOC. Both were proposed for splitting during the v0.1.0 hardening pass. A direct read of the structure surfaces the same kind of finding the cli-tools extraction doc records: the cut is harder than it looks from the outside.

### zruntime.zig

The file groups into roughly six bands:

- **Runtime lifecycle and request entry-point** (`:1-2367`) - the `Runtime` struct, hidden class pool, native binding registration, request dispatch.
- **Headers / Request / Response object construction** (`:2368-2725`) - `FetchResponseObjects`, `OwnedResponseHeader`, `OwnedResponseHead`, the `headers*Native` family, `Response.*` static wrappers, `requestConstructorNative`.
- **Fetch implementation** (`:2726-3055`) - `splitHeaderKV`, `outboundHostViolation`, `parseFetchArgs`, `parseFetchInitOptions`, `fetchSyncResult`, `collectFetchForParallel`.
- **Durable + IO + WebSocket + Service callbacks** (`:3101-3960`) - the bridge between JS callbacks and Zig runtime state.
- **IO worker, HTTP request native, console** (`:3961-4500`).
- **Test scaffolding + 47 tests** (`:4537-7048`) - `TestHttpServer`, `TestCapturedRequest`, `captureRequest`, `makeTestRequest`, and the test bodies that consume them.

Two seams that look clean from outside collapse on contact:

1. **Test scaffolding split.** Zig tests are file-local. To move `TestHttpServer` and the capture helpers into a sibling `zruntime/test_server.zig`, every type the tests consume - currently 18 `const` declarations - must become `pub const` and ride in the public API of the runtime package. That puts test machinery on the user-facing surface area and is worse than the original duplication.
2. **Fetch / response / headers split.** Each of these groups reaches into `Runtime` struct private fields: the hidden class pool, the percentile trackers, the bytecode function tracker, the AOT override hook, the durable fetch cache. Extracting to siblings forces either (a) widening dozens of fields to `pub`, or (b) introducing a `RuntimeContext` view struct passed to every helper. Both are bigger changes than the line-count reduction warrants.

### interpreter.zig

The file already has substantial sibling factoring under `packages/zigts/src/interpreter/` (`alloc`, `arith`, `call`, `cmp`, `env_cache`, `frame`, `ic`, `jit_compile`, `jit_intrinsics`, `jit_policy`, `lifecycle`, `perf`, `saved_state`, `trace`, `util`). The 5,398 lines that remain are the dispatch core plus opcode bodies that share heavy state with the dispatch loop's switch fallthroughs. Splitting `dispatch.zig` / `stack_ops.zig` / `object_ops.zig` / `call_ops.zig` requires either (a) moving the entire main switch into a generic dispatcher and demoting opcode bodies into top-level functions (cost: every opcode that currently uses `continue` / `break` against the loop must be rewritten to return a control-flow signal), or (b) keeping the switch in `interpreter.zig` and only moving leaf helpers (cost: ~10% LOC reduction with no real seam improvement).

## The seam, for v0.2.0

### zruntime.zig

Three pre-work commits make the split tractable:

1. Introduce `RuntimeContext` - a view struct that bundles the eight `Runtime` fields the fetch / response / headers / durable code reads. Pass it through instead of `*Runtime`.
2. Move the test scaffolding into `packages/runtime/src/testing/` as a new `pub`-surfaced helper module gated by a `build_options.test_helpers` flag. The user-facing runtime package does not advertise it.
3. Once `RuntimeContext` exists, the fetch / response / headers split is a near-mechanical move into `packages/runtime/src/zruntime/{fetch,response,headers,durable}_native.zig`.

### interpreter.zig

Demote each opcode body to a function returning `DispatchControl` (an enum: `next`, `branch`, `return_value`, `throw`). The main switch in `dispatch.zig` reads the control signal and updates `pc` / `sp` accordingly. After that mechanical change, the body files (`stack_ops`, `object_ops`, `call_ops`) extract cleanly because each function has a uniform return shape.

## What's in v0.1.0 instead

The visible-win dedup in the same area landed during the pre-1.0 hardening pass:

- `server.zig` `isFramingHeader` helper unified two near-duplicate "skip Content-Length and Connection" loops.
- `server.zig` `formatHttpError` is now the single source of truth for HTTP error response framing; `sendErrorSync` and `sendErrorResponse` delegate to it.

The proof-ledger duplication the original audit flagged was already fixed: both `live_reload.zig` (`appendLedgerEntry`) and `proofs_cli.zig` route through `proof_ledger.appendEvent`. The CLI arg-parse loops the audit also flagged are being deduped surgically per the user's preferred style (`ecb8c33`, `7a3addc`, `01ebdbf`, `dfc33ab`); a generic iterator abstraction would conflict with that style.

## Verification at v0.2.0

After the splits land:

- `wc -l packages/runtime/src/zruntime.zig` should drop below 2,000.
- `wc -l packages/zigts/src/interpreter.zig` should drop below 1,500.
- No symbol moves to `pub` solely to satisfy a cross-file reference - everything that becomes `pub` must be intended for user code.
- The runtime test suite count (currently 47 in zruntime.zig) is preserved across the split with no behavior diffs.

## Tied to this split: HandlerPool.init FailingAllocator coverage

The v0.1.0 hardening pass added FailingAllocator walkers for `Runtime.create` (`packages/zigts/src/pool.zig` test "Runtime.create errdefer ladder closes every failure path"), `parseContractJson` (`packages/runtime/src/contract_runtime.zig`), and the precompile build path. `HandlerPool.initWithEmbeddedAndDeps` (`packages/runtime/src/runtime_pool.zig:122`) now has targeted FailingAllocator coverage through the zruntime test "HandlerPool init under FailingAllocator never leaks". That test covers the native binding ownership bugs found while unskipping the harness, the parser scope OOM path, and a successful initialization probe.

The full fail-index sweep remains opt-in with `ZTS_RUN_OOM_SWEEP=1 zig build test-zruntime` because `prewarm()` reaches the full parse/compile path. Once the test scaffolding moves into `packages/runtime/src/testing/` per step 2 of the v0.2.0 split, the same harness can move out of `zruntime.zig` and closer to `runtime_pool.zig`.
