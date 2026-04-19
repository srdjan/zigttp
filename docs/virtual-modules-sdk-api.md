# Virtual-Module SDK API: Inventory and Phase 0 Design

## Status

Phase 0 deliverable for the peer-package move (`docs/virtual-modules-peer-package.md`). Written 2026-04-19.

## Source inventory

The 22 built-in virtual modules live under `packages/zigts/src/modules/` (8,161 lines total), already grouped to match `docs/virtual-modules/`:

| Group | Files |
|---|---|
| `internal/` (not published) | compiler, file_resolver, module_graph, resolver, types, util |
| `security/` | auth, crypto, decode, validate |
| `data/` | cache, ratelimit, sql |
| `net/` | fetch, service, websocket |
| `http/` | http_mod, router, url |
| `workflow/` | compose, durable, io, scope |
| `platform/` | env, id, log, text, time |

The grouping matches the target layout, so the move is a lift-and-shift of files plus an import rewrite. The folder `internal/` contains the private module-registry machinery (`resolver`, `module_graph`, `file_resolver`, `compiler`, `types`, `util`). `resolver` and `compiler` belong with parser/bytecode machinery and stay in zigts per the plan's non-goals; `util` is what the modules package needs. `module_graph`, `file_resolver`, `types` are consumed outside the modules folder and stay in zigts.

## What the SDK already owns

`packages/zigttp-sdk/src/root.zig` today:

- `JSValue` (NaN-boxed, packed struct)
- `ModuleBinding`, `FunctionBinding`
- `ModuleCapability` (10 of 11 variants; see drift below)
- `ReturnKind`, `EffectClass`, `FailureSeverity`
- `DataLabel`, `LabelSet`
- `ContractCategory` (10 of 16 variants; see drift below)
- `ContractTransform`, `ContractExtraction`, `ContractFlags`
- `hasCapability`, `requireCapability`, `nowMs`, `fillRandom`, `writeStderr`
- `validateBindings` (restricts specifiers to `zigttp-ext:` only)

The SDK registers four `extern fn zigttpSdk*` hooks. The zigts side defines the exports inside `packages/zigts/src/module_binding.zig`, which is the runtime's canonical copy of the binding spec.

## Drift between zigts and SDK

Zigts's `module_binding.zig` is the live copy. The SDK copy is a subset. Aligning them is part of Phase 1:

| Symbol | Zigts has | SDK missing |
|---|---|---|
| `ModuleCapability.websocket` | yes | yes |
| `ContractCategory` | adds `scope_name`, `cookie_name`, `cors_origin`, `rate_limit_key`, `service_call`, `fetch_host` | yes |
| `ContractFlags.sets_scope_used` | yes | yes |
| `Law`, `LawKind`, `AbsorbingPattern`, `FunctionBinding.laws` | yes | yes |
| `ModuleBinding.self_managed_io` | yes | yes |
| `validateBindings` accepts `zigttp:` prefix | yes | no (SDK rejects it) |

## What every module imports today

Usage counts from inspecting the 22 module files:

| Internal import | Used by | Role |
|---|---|---|
| `@import("../../context.zig")` | all 22 | allocator, GC, module state, exception slot, capability policy, globals |
| `@import("../../value.zig")` | all 22 | `JSValue` (already SDK-owned) |
| `@import("../../module_binding.zig")` | all 22 | `ModuleBinding`, capability-checked helpers (`allowsEnvChecked`, `allowsCacheNamespaceChecked`, `allowsSqlQueryChecked`, clock, random, crypto, stderr) |
| `@import("../internal/resolver.zig")` | all 22 | `ModuleExport` legacy projection |
| `@import("../internal/util.zig")` | all 22 | `castContext`, `extractString`, `extractInt`, `extractFloat`, `throwError`, `throwTypeError`, `throwCapabilityPolicyError`, `createPlainResult*` |
| `@import("../../object.zig")` | 15 | `JSObject`, `NativeFn` signature, property set/get |
| `@import("../../string.zig")` | 13 | `JSString`, `SliceString`, `RopeNode` |
| `@import("../../module_slots.zig")` | 9 (stateful) | `Slot` enum |
| `@import("../../sqlite.zig")` | 1 (`sql`) | `sqlite.Db`, open/close/exec |
| `@import("../../arena.zig")` | 3 (`service`, `fetch`, `scope`) | per-request scratch buffers |
| `@import("../../gc.zig")`, `heap.zig` | tests only | test-harness initialization |
| `@import("../../http.zig")` | 2 (`router`, `http_mod`) | header parsing helpers, cookie encoder |
| `@import("../../security_events.zig")` | 2 | structured security events |
| `@import("../../compat.zig")` | 4 | `realtimeNowMs`, `realtimeNowNs` (already surfaced via `nowMs` in SDK) |
| `@import("../../trace.zig")` | 2 (via util/durable) | trace write hooks (already wrapped by registration layer) |

The `ModuleFn` opaque-handle ABI (`*ModuleHandle` instead of `*anyopaque`) already exists in `module_binding.zig`. Current built-ins use `FunctionBinding.func = NativeFn` (raw `*anyopaque`). Third-party extensions use `FunctionBinding.module_func = ModuleFn`. The port converts all 22 modules from `func` to `module_func` and adds the missing SDK surface those modules need.

## Decisions locked for Phase 1

### 1. V-table shape

Fat-pointer handle, matching the current `zigttp-ext-demo` usage: `ModuleHandle` stays an opaque type, and every SDK free function takes `*ModuleHandle` as first argument. Internally zigts casts it to `*Context`. No explicit `{ ptr, vtable }` struct; the cast is authorized via `pub fn handleToContext` on the zigts side.

Rationale: thin-wrapper v-tables would add indirection at every allocator/object op with no benefit given both zigts and modules live in the same binary. The opacity (`opaque {}`) already prevents modules from reaching past the ABI. Third-party extensions loaded as separate objects (future) will need a true v-table; that is a follow-up and explicitly out of scope here.

### 2. Memory ownership

- `createString(handle, bytes)` allocates via the zigts GC. The returned `JSValue` is GC-rooted for the current function call (the module's stack frame). The module does not own the backing bytes.
- `createObject(handle)` returns a GC-rooted `JSValue` that owns its own property table.
- Module state allocated via `getAllocator(handle)` uses the runtime's long-lived allocator; the module frees it in `state_deinit`.
- Borrowed slices returned by `extractString(val)` are valid only while `val` is live on the caller's stack.

### 3. Test harness

Module tests today spin up `GC` + `Heap` + `Context` by hand. After the port, modules package tests use `sdk.test_harness.init(allocator)` which returns a live `ModuleHandle` backed by a real zigts `Context`. Harness lives in `packages/zigttp-sdk/src/test_harness.zig`. It is a thin wrapper that depends on `zigts`; dependency direction is `modules -> zigttp-sdk`, and `zigttp-sdk.test_harness` imports `zigts` only when compiled for tests (`if (builtin.is_test)`), or is a separate test-only submodule exposed via a build option.

Proposal for the build graph: expose `zigttp-sdk-test` as a second module in `packages/zigttp-sdk/build.zig` that depends on both `zigttp-sdk` and `zigts`. The modules package's `test` step imports `zigttp-sdk-test`; its `runtime` step imports only `zigttp-sdk`. This keeps the production dependency graph clean and avoids a circular deps at compile time.

### 4. Classification of every internal symbol

| Symbol | Ownership after refactor | Notes |
|---|---|---|
| `JSValue` | SDK (already) | no change |
| `ModuleBinding`, `FunctionBinding` | SDK (expanded) | SDK becomes canonical; zigts re-exports as aliases |
| `ModuleCapability` | SDK (expanded to 11) | zigts alias |
| `ContractCategory`, `ContractFlags`, `ContractExtraction`, `ContractTransform` | SDK (expanded) | zigts alias |
| `DataLabel`, `LabelSet` | SDK (already) | no change |
| `ReturnKind`, `EffectClass`, `FailureSeverity` | SDK (already) | no change |
| `Law`, `LawKind`, `AbsorbingPattern` | SDK (new) | zigts alias |
| `Slot` (module_slots.zig) | SDK (new) | reserved slot indices; third-party modules do not allocate slots |
| `validateBindings` | SDK (relaxed) | accepts both `zigttp:` and `zigttp-ext:` prefixes |
| `ModuleHandle`, `ModuleFn`, capability helpers | SDK (already) | no change |
| `createString`, `createObject`, `defineProperty`, `get`, `set`, `throwError`, `getState`, `setState`, `getAllocator` | SDK (new) | free functions taking `*ModuleHandle` |
| `Sqlite.Db` opaque | SDK (new) | thin wrapper; implementation in zigts |
| `allowsEnvChecked`, `allowsCacheNamespaceChecked`, `allowsSqlQueryChecked` | SDK (new) | capability-policy ABI |
| `security_events.emitGlobal` | SDK (new thin wrapper) | `emitSecurityEvent(handle, kind, subject, detail)` |
| `compat.realtimeNowMs/Ns` | covered by SDK `nowMs` (already) | already behind capability check |
| `arena.Arena` | **stays in zigts**. modules that need request-scoped scratch space get `sdk.requestArena(handle)` returning `std.mem.Allocator`. | |
| `object.JSObject`, `string.JSString`, `string.SliceString`, `string.RopeNode` | **stay in zigts**. Modules hold `JSValue` only; string/object access goes through SDK free functions. | |
| `context.Context`, `gc.GC`, `heap.Heap` | **stay in zigts**. Never seen by modules. | |
| `resolver.ModuleExport` | **stays in zigts**. Runtime-only legacy projection. SDK registers `ModuleBinding`; zigts converts internally. | |
| `parser`, `stripper`, `type_pool`, `type_env`, `builtin_modules` | **stay in zigts**. Contract builder reads `ModuleBinding` metadata from comptime. Modules never touch parser types. | |
| `trace.zig` hooks | **stays in zigts**. Registration layer wraps traceable functions transparently. | |
| `http.zig` helpers (cookie encoder, content-type parse) | **stays in zigts**, re-exported via SDK as pure helpers (no handle required). | |

### 5. ABI additions required

The SDK gets these new files:

- `context.zig` — handle free functions: `getAllocator`, `getModuleState`, `setModuleState`, `requestArena`.
- `object.zig` — `createObject(handle)`, `objectSet(handle, obj, key, val)`, `objectGet(handle, obj, key)`, `objectDefineProperty(handle, obj, key, val, flags)`.
- `string.zig` — `createString(handle, bytes)`, `extractString(val)` (already in handle fn list; move here), plus rope-flattening helper `extractStringFlat(handle, val)`.
- `binding.zig` — move `ModuleBinding` and `FunctionBinding` here from `root.zig`.
- `slot.zig` — `Slot` enum copy.
- `helpers.zig` — `resultOk`, `resultErr`, `resultErrValue`, `resultErrs`, `throwError`, `throwTypeError`, `throwCapabilityPolicyError`.
- `errors.zig` — capability and policy error types; `emitSecurityEvent`.
- `sqlite.zig` — opaque `Db` with `open`, `close`, `prepare`, `execute`, `bind`, `step`, `finalize`. Thin wrapper around `packages/zigts/src/sqlite.zig`.
- `test_harness.zig` — `init(allocator) -> Harness`, `Harness.handle()`, `Harness.deinit()`.
- `laws.zig` — `Law`, `LawKind`, `AbsorbingPattern`.

Zigts fills the v-table by defining `pub export fn zigttpSdk*` entry points in `sdk_impl/` (or extending the existing exports in `module_binding.zig`). New exports needed:

- `zigttpSdkCreateString(handle, ptr, len, *out) -> bool`
- `zigttpSdkCreateObject(handle, *out) -> bool`
- `zigttpSdkObjectSet(handle, obj, key_ptr, key_len, val) -> bool`
- `zigttpSdkObjectGet(handle, obj, key_ptr, key_len, *out) -> bool`
- `zigttpSdkGetModuleState(handle, slot) -> *anyopaque`
- `zigttpSdkSetModuleState(handle, slot, ptr, deinit_fn)`
- `zigttpSdkThrowError(handle, name_ptr, name_len, msg_ptr, msg_len) -> JSValue`
- `zigttpSdkAllocator(handle) -> *std.mem.Allocator` (pointer stable for runtime lifetime)
- `zigttpSdkAllowsEnv(handle, name_ptr, name_len) -> bool`
- `zigttpSdkAllowsCacheNamespace(handle, name_ptr, name_len) -> bool`
- `zigttpSdkAllowsSqlQuery(handle, name_ptr, name_len) -> bool`
- `zigttpSdkSqliteOpen(handle, path_ptr, path_len, *out) -> i32`
- (plus sqlite prepare/step/bind/finalize exports)

Fat-pointer avoidance: these are all plain `extern fn` exports. The modules package imports the SDK; the SDK imports nothing from zigts; zigts pulls the SDK into its own build to gain the `pub extern` table.

## Risks surfaced during inventory

- **`validate.zig` and `decode.zig` reach into `type_pool`, `type_env`, `builtin_modules` for compile-time contract integration.** Inventory pass says these imports are actually in the *registration* layer (`builtin_modules.zig`), not the modules themselves. Confirm per-module before Phase 3-7 starts; any exception gets a module-specific shim.
- **`http_mod.zig` uses `http.zig` directly for cookie encoding.** `http.zig` also owns request/response types; the modules package needs only the pure helpers. Extract `cookies.zig` / `content_type.zig` as pure helpers in zigts and re-export via SDK.
- **`durable.zig` holds a `DurableCallbacks` struct with `*const fn` pointers taking `*context.Context`.** Port switches the callback signatures to `*ModuleHandle`; the runtime layer updates correspondingly when installing callbacks. Same pattern for `service.zig` and `fetch.zig`.
- **`security_events.emitGlobal` writes to a process-global sink.** Wrap it in a handle-free SDK function (`emitSecurityEvent(kind, subject, detail)`) that reads the active-module context from the same threadlocal zigts already uses.
- **The existing SDK's `validateBindings` rejects `zigttp:` prefix.** Relaxing this is mandatory in Phase 1; the zigts-side `validateBindings` already supports both prefixes.

## What is not addressed here

- SDK versioning scheme (non-goal per the parent plan).
- Third-party dynamic-loading ABI (out of scope).
- Changes to `docs/virtual-modules/*.md` content (non-goal).
- Any behavior change visible to handler code (non-goal).

## Ready-to-execute Phase 1 checklist

Ordered by dependency; each item is a self-contained commit.

1. Add missing `ModuleCapability.websocket`, extra `ContractCategory` variants, `ContractFlags.sets_scope_used`, `Law`/`LawKind`/`AbsorbingPattern`, `ModuleBinding.self_managed_io`, and accept `zigttp:` prefix in `validateBindings`. Zigts re-exports via `pub const ModuleCapability = sdk.ModuleCapability;` etc. All existing tests pass.
2. Split SDK `root.zig` into `binding.zig`, `laws.zig`, `slot.zig`, `helpers.zig`, `errors.zig`. `root.zig` re-exports for backward compat.
3. Add `context.zig` handle ops; wire `extern fn` exports in zigts `sdk_impl/context_impl.zig`.
4. Add `object.zig` and `string.zig` handle ops; wire exports.
5. Add `sqlite.zig` opaque handle; wire exports from `packages/zigts/src/sqlite.zig`.
6. Add `test_harness.zig` as a separate module in the SDK build; verify a synthetic "empty" module can be loaded and called via the handle ABI.

On the final commit of Phase 1, `zig build test --summary all` reports the same counts as the pre-refactor baseline. Phase 2 can proceed.
