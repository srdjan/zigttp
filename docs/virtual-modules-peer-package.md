# Design: Virtual Modules as a Peer Package

## Status

Proposal. Not implemented. Written 2026-04-19.

## Goal

Move the 22 built-in virtual modules out of `packages/zigts/src/modules/` and into a standalone peer package `packages/modules/` (sibling of `zigts`, `runtime`, `tools`). The folder layout should mirror `docs/virtual-modules/`:

```
packages/modules/
├── build.zig
├── build.zig.zon
├── src/
│   ├── root.zig             (analog of docs/virtual-modules/README.md)
│   ├── internal/            (util, state helpers)
│   ├── security/            (auth, crypto, validate, decode)
│   ├── data/                (cache, sql, ratelimit)
│   ├── net/                 (fetch, service, websocket)
│   ├── http/                (router, http, url)
│   ├── workflow/            (durable, compose, scope, io)
│   └── platform/            (env, id, log, text, time)
└── module-specs/
    └── {security,data,net,http,workflow,platform}/*.json
```

## Why this is hard

The 22 virtual modules currently reach deep into zigts internals:

| Internal import | Consumers | Purpose |
|---|---|---|
| `context.Context` | all 22 | allocator, GC, heap, module state slots, globals |
| `value.JSValue` | all 22 | NaN-boxed JS value (already in `zigttp-sdk`) |
| `object.Object`, `string.String` | ~15 | heap-allocated values returned to JS |
| `module_binding.ModuleBinding` | all 22 | binding declaration schema |
| `module_slots.Slot` | stateful modules (6) | per-module state slot enum |
| `builtins/helpers.zig` | ~10 | option/result helper constructors |
| `sqlite.zig` | `sql.zig` only | SQLite handle |
| `gc.GC`, `heap.Heap`, `arena.Arena` | tests only | runtime initialization in unit tests |
| `parser`, `stripper`, `type_pool`, `type_env`, `builtin_modules` | `decode.zig`, `validate.zig` | compile-time contract integration |

That spread creates a circular dependency if both packages import each other:

- `packages/modules/` needs `context`, `object`, `sqlite`, etc. from zigts.
- `packages/zigts/src/builtin_modules.zig` needs the 22 bindings from modules to register them.
- Zig's build graph rejects circular package deps.

The existing extension pattern (`zigttp-ext-demo`) sidesteps this by using `zigttp-sdk` — a stable ABI layer that exposes only opaque handles and packed-struct value types. Extensions never see zigts internals.

**The peer-package move is feasible only if virtual modules switch from zigts internals to the SDK ABI.**

## Approach

Port modules to consume `zigttp-sdk` instead of zigts internals. Expand the SDK to cover the new surface. Zigts imports the bindings back from the new package; it does not import its own internals through the SDK.

Dependency graph afterward:

```
zigttp-sdk  (pure types + opaque handles)
   ▲   ▲
   │   └───── zigts        (runtime + compiler, provides v-table impls for SDK handles)
   │          ▲
   └── modules    (virtual modules, uses SDK handles)
                  ▲
                  └── zigts-builtin_registry (wires modules into zigts)
```

The "builtin_registry" is either a new glue file in zigts (the natural home, same as today's `extension_bindings.zig`) or a shim package. Default: keep it inside zigts, where `builtin_modules.zig` already lives.

## Phases

Each phase lands as its own commit on a green build.

### Phase 0: Inventory and SDK shape

Deliverable: `docs/virtual-modules-sdk-api.md` — a table of every type the modules need, categorized as:

- **SDK owns outright** (moves from zigts into SDK): `JSValue` (already there), `ModuleBinding`, `FunctionBinding`, `Slot`, option/result helpers, effect/severity enums.
- **SDK exposes as opaque handle + v-table**: `Context` (ops: `allocator`, `createString`, `createObject`, `getModuleState`, `setModuleState`, `throwError`, `callFunction`), `Object` (ops: `get`, `set`, `defineProperty`), `String` (ops: `bytes`, `len`).
- **SDK exposes as opaque handle only**: `Sqlite.Db` (used by one module; thin wrapper).
- **Stays in zigts** (not needed at runtime by modules): `parser`, `stripper`, `type_pool`, `type_env`, `builtin_modules`. Contract integration happens compile-time via the contract builder, which is already in zigts and reads binding metadata through `ModuleBinding` (SDK-owned). Modules don't directly import parser types.

Design decisions to lock before Phase 1:

1. **V-table shape**. Fat pointer (Context = `{ ptr: *anyopaque, vtable: *const ContextVTable }`) vs thin-pointer-plus-extern-fns. Recommendation: fat pointer, matches `zigttp-ext-demo`'s existing usage.
2. **Memory ownership**. `createString(ctx, bytes)` returns a handle — who owns the backing memory? Proposal: zigts GC owns; SDK users hold opaque handles that are root-safe for the scope of the call.
3. **Tests**. Modules currently have unit tests that spin up a real `GC` + `Heap` + `Context`. After the port, tests spin up zigts via the SDK's context factory, or tests move into zigts's test suite. Recommendation: keep tests in the modules package; SDK exposes a `TestHarness` helper that wraps real zigts initialization.

### Phase 1: Expand `zigttp-sdk`

Add:

- `packages/zigttp-sdk/src/context.zig` — `Context` handle + v-table
- `packages/zigttp-sdk/src/object.zig` — `Object` handle + v-table
- `packages/zigttp-sdk/src/string.zig` — `String` handle + v-table
- `packages/zigttp-sdk/src/binding.zig` — move `ModuleBinding`, `FunctionBinding` out of zigts
- `packages/zigttp-sdk/src/slot.zig` — `Slot` enum (move from zigts `module_slots.zig`)
- `packages/zigttp-sdk/src/helpers.zig` — option/result constructors (move from zigts `builtins/helpers.zig`)
- `packages/zigttp-sdk/src/errors.zig` — `throwError(ctx, class, message)` wrapper
- `packages/zigttp-sdk/src/sqlite.zig` — opaque `Db` type for `sql.zig`
- `packages/zigttp-sdk/src/test_harness.zig` — factory that produces a live context for module tests

Zigts sprouts implementation glue that fills in the v-tables:

- `packages/zigts/src/sdk_impl/context_impl.zig` (or inline in `context.zig`)
- Same for object, string, sqlite

Zigts imports `ModuleBinding`, `FunctionBinding`, `Slot`, helpers *back from the SDK* rather than owning them. This is a one-way flow: SDK owns the API, zigts provides the implementation.

Tests: zigts's existing tests still pass because the types are backward-compatible at the Zig type level (type aliases).

### Phase 2: Create `packages/modules/` scaffolding

- `packages/modules/build.zig` — declares a `zigttp_modules` module, depends on `zigttp-sdk` only
- `packages/modules/build.zig.zon` — deps: `zigttp-sdk` (path `../zigttp-sdk`)
- `packages/modules/src/root.zig` — re-exports the 22 bindings
- `packages/modules/src/internal/util.zig` — ported `util.zig`, using SDK types
- Empty group folders ready to receive modules.

### Phase 3-7: Port modules by group

Order: ascending complexity.

**Phase 3 (security)**: `crypto` (purest) → `auth` → `validate` (stateful registry) → `decode` (depends on validate). One commit per module once tests pass.

**Phase 4 (platform)**: `env` → `id` → `text` → `time` → `log`. All simple.

**Phase 5 (http)**: `router` → `url` → `http`. Mostly pure functions.

**Phase 6 (data)**: `ratelimit` → `cache` → `sql` (complex: opens sqlite handle, holds connection state).

**Phase 7 (net/workflow)**: `compose` → `scope` → `io` → `service` → `fetch` → `websocket` → `durable` (most complex: oplog, callbacks).

For each module:
1. Move file + rewrite imports to use SDK.
2. Rewrite state access (`ctx.getModuleState(X, Slot.foo)` already works through the v-table).
3. Update tests: `var gc = try GC.init(...)` becomes `var harness = try test_harness.init(...)`.
4. `zig build test-modules` green before commit.

### Phase 8: Wire up in zigts

- Update `packages/zigts/build.zig.zon`: add `zigttp_modules` as dep.
- Update `packages/zigts/build.zig`: import the new module, expose to downstream.
- Rewrite `packages/zigts/src/builtin_modules.zig`:
  - `@import("modules/security/auth.zig")` → `@import("zigttp_modules").security.auth`
  - Governance entries update to `packages/modules/src/security/auth.zig` and `packages/modules/module-specs/security/auth.json`.
- Rewrite 5 external callers (`module_binding.zig`, `trace.zig`, `http.zig`, `file_io.zig`, `module_audit.zig`) — these import `util.zig` specifically; after util moves into the modules package, they import from the SDK's helpers (if appropriate) or continue to reach through the `zigttp_modules` import.
- Delete `packages/zigts/src/modules/` entirely.

### Phase 9: Specs + docs + tooling

- `git mv packages/zigts/module-specs/ packages/modules/module-specs/`.
- Update `builtin_modules.zig` spec_path strings.
- Update `packages/tools/src/module_audit.zig` path constants and test fixtures (the audit enforces paths under `packages/zigts/src/modules/`; change to `packages/modules/src/`).
- Update `scripts/check-capability-helpers.sh`: `git ls-files 'packages/zigts/src/modules/*.zig'` → `git ls-files 'packages/modules/src/**/*.zig'`.
- Update the root `README.md` architecture paragraph: virtual modules are their own package.
- Update `docs/architecture.md` if it references the old modules path.
- `docs/virtual-modules/*` needs no changes (docs live by content, not code path).

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| V-table indirection regresses hot-path perf | Measure before/after with the bench harness (`packages/runtime/src/benchmark.zig`). Context ops are called at module entry only, not per-instruction. Expected overhead: one indirect call per module call. Comparable to calling through a function pointer — nanoseconds. |
| SDK API shape forecloses future zigts internal changes | Version the SDK. Breaking changes bump the SDK version. Modules pin to the SDK version. |
| Tests for stateful modules (sql, durable) need real zigts runtime | SDK `test_harness.zig` provides it; tests stay black-box. |
| Contract builder needs to see module binding metadata at compile time | Already works — `ModuleBinding` is comptime-addressable from the SDK-owned declaration. |
| The `builtins/helpers.zig` move breaks in-zigts callers | Zigts imports helpers from the SDK after the move; one grep-and-replace pass. |
| Intermediate phases leave the build broken | Each phase is its own commit on a green build. Phase 1 is a no-op for zigts (types still aliased). Phase 3-7 are per-module, each one green. |

## Acceptance criteria

- `packages/zigts/src/modules/` deleted.
- `packages/modules/` contains the 22 virtual modules + specs in the 6 group folders, matching `docs/virtual-modules/` shape.
- `zig build test --summary all` reports **≥ 1503 passing, 4 skipped** (baseline before any work began) on the final commit.
- `git log --oneline` shows one commit per phase, each one building cleanly in isolation (`git bisect` friendly).
- `scripts/check-capability-helpers.sh` still enforces no direct zigts-internal sensitive-op calls from the modules package (update the glob).
- Third-party extension ergonomics unchanged: `zigttp-ext-demo` continues to work.

## Effort estimate

3-5 days of focused work. Eight to ten commits:

- Phase 0: half-day (design doc + inventory)
- Phase 1: 1 day (SDK expansion + zigts glue)
- Phase 2: 1-2 hours (scaffolding)
- Phases 3-7: 2-3 days (porting by group, tests, commits)
- Phase 8: 2-4 hours (wire-up)
- Phase 9: 2-4 hours (cleanup and tooling)

## Explicit non-goals

- No behavior changes to handler-facing virtual-module APIs.
- No changes to `docs/virtual-modules/*.md` content (docs are the source of truth; they already describe modules by behavior, not by file path).
- No SDK versioning scheme in this refactor — first-cut commits tag `zigttp-sdk` version `0.1.0` and zigts pins to it. Versioning policy is a separate concern.
- No runtime registration of built-in modules (they remain comptime-registered via `builtin_modules.zig`).
- No split of `resolver.zig` or `compiler.zig` out of zigts — those belong with the parser/bytecode machinery.

## Alternatives considered

1. **In-place rename** (`packages/zigts/src/modules/` → `.../virtual-modules/`). Rejected because the folder parent still doesn't match docs — `packages/zigts/src/virtual-modules/` vs `docs/virtual-modules/` is one layer off. Also doesn't decouple.
2. **Workspace include trick** — keep the files as part of zigts package but physically move them under `packages/modules/`. Rejected because the decoupling is cosmetic; imports stay `@import("../../context.zig")` with one more `../`; doesn't enable third-party module development.
3. **Move zigts internals into SDK wholesale**. Rejected because the SDK would then own implementation, not an API. SDK should be stable for third parties; internal types shift often.
