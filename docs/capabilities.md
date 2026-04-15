# Module Capabilities

Virtual modules exposed through `zigttp:*` imports run inside the handler's JavaScript context but often need to touch external resources (clock, RNG, filesystem, network). Capabilities record exactly which resources each module's Zig implementation is allowed to reach; an implementation that reaches outside its declaration panics at build time rather than silently misbehaving.

Read this end-to-end before adding a new virtual module.

## The capability enum

`ModuleCapability` is declared in [`packages/zigts/src/module_binding.zig`](../packages/zigts/src/module_binding.zig) (around line 501). Ten variants exist today:

| Capability | Gates |
|---|---|
| `env` | Reading process environment variables (used by sandbox policy checks, not by the module's own config). |
| `clock` | Real wall-clock and monotonic time (`std.time.timestamp`, `std.time.milliTimestamp`, monotonic counters). |
| `random` | OS-seeded cryptographically weak randomness (UUID generation, nanoid, jitter). |
| `crypto` | Cryptographic primitives (HMAC, hash functions, constant-time comparison, JWT sign/verify). |
| `stderr` | Writing to the process stderr stream for diagnostic output. |
| `runtime_callback` | Invoking the host runtime back into JS (scope lifecycle hooks, durable oplog replay, I/O dispatch, service routing). |
| `sqlite` | Opening and querying the embedded SQLite connection. |
| `filesystem` | Reading files from disk outside the sandbox root (service contracts, request fixtures). |
| `network` | Making outbound network calls. |
| `policy_check` | Consulting the handler's derived `RuntimePolicy` to authorize a resource access before it happens. |

These are governance metadata for the module internals. They do not affect handler-level effect classification (`deterministic`, `read_only`, etc.) or `RuntimePolicy` derivation; those are separate analyses driven by the `effect` annotation on each exported function.

## Enforcement model

Every module declares a `ModuleBinding` with three relevant fields:

```zig
.specifier = "zigttp:foo",
.effect = .read,
.required_capabilities = &.{ .clock, .policy_check },
```

Two cooperating helpers in `module_binding.zig` turn the declaration into an enforcement contract:

1. **`wrapNativeFnWithCapabilities(user_fn, specifier, required_capabilities)`** is a comptime wrapper that the resolver ([`packages/zigts/src/modules/resolver.zig:164`](../packages/zigts/src/modules/resolver.zig)) applies automatically to every exported function of a module whose `required_capabilities` list is non-empty. The wrapper pushes an `ActiveModuleContext` onto a thread-local slot before calling the user function and pops it on exit. Modules with an empty capability list skip the wrapper entirely at compile time, so there is zero runtime overhead for capability-free modules (`zigttp:text`, `zigttp:compose`, etc.).

2. **`requireCapability(handle, capability)`** is the call-site check. Inside a module implementation, any operation that touches the guarded resource calls `requireCapability(handle, .clock)` (or the read-only sibling `hasCapability`). The check inspects the thread-local `active_module_context` and returns `error.MissingModuleCapability` if the capability is not present. Because the wrapper is applied by the resolver, an implementation that calls a guarded helper without the declaration in its binding will panic in the build-time `test-capability-audit` pass before reaching runtime.

The thread-local state (`threadlocal var active_module_context` at `module_binding.zig:54`) is the exact reason capability enforcement composes correctly with the `HandlerPool`: each worker thread carries its own context stack, no locks needed, and nested module calls (A calls B) save and restore via the `ActiveModuleToken` returned from `pushActiveModuleContext`.

Modules that also need manual pushes (for example, when a helper runs before the wrapped entry point) import the binding helpers directly:

```zig
const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
defer mb.popActiveModuleContext(token);
```

This pattern is visible in `id.zig`, `env.zig`, `sql.zig`, `service.zig`, `cache.zig`, `ratelimit.zig`, `auth.zig`, `io.zig`, and `scope.zig`.

## Module inventory

### Modules that declare capabilities

| Specifier | Capabilities | Notes |
|---|---|---|
| `zigttp:auth` | `crypto`, `clock` | JWT sign/verify, bearer parsing, timing-safe equality, HMAC webhook verification. |
| `zigttp:cache` | `clock`, `policy_check` | TTL expiry requires clock; `policy_check` consults the handler's cache-namespace policy. |
| `zigttp:crypto` | `crypto` | SHA256, HMAC, base64. |
| `zigttp:durable` | `runtime_callback` | Replay and live execution dispatch back into the runtime for oplog replay and signal wake-ups. |
| `zigttp:env` | `env`, `policy_check` | Reads `getenv` and then checks the key against the handler's env allowlist. |
| `zigttp:id` | `clock`, `random` | UUID v7 and ULID mix clock; nanoid is pure random. |
| `zigttp:io` | `runtime_callback` | `parallel()` and `race()` schedule outbound fetches through the runtime's I/O collector. |
| `zigttp:log` | `clock`, `stderr` | Timestamped log emission. |
| `zigttp:ratelimit` | `clock` | Token bucket expiry. |
| `zigttp:scope` | `runtime_callback` | Request-scoped lifecycle hooks call back into the runtime at request end. |
| `zigttp:service` | `filesystem`, `runtime_callback` | Reads cross-handler service contracts from disk and dispatches via the runtime. |
| `zigttp:sql` | `sqlite`, `policy_check` | SQLite connection plus query-name allowlist check. |

### Modules that declare no capabilities

These modules are pure compute - string manipulation, parsing, URL encoding, structural routing, type-directed decoding - and run without a wrapper. They appear in `modules/` with a `ModuleBinding` whose `required_capabilities = &.{}` or that omits the field.

- `zigttp:compose`
- `zigttp:decode`
- `zigttp:http`
- `zigttp:router`
- `zigttp:text`
- `zigttp:time`
- `zigttp:url`
- `zigttp:validate`

The absence of a capability does not make these modules "trusted": they still go through the resolver and still respect the handler's effect classification. It means only that their implementation does not touch any resource on the guarded list.

## The build-time audit

`zig build test-capability-audit` grep-walks `packages/zigts/src/modules/` looking for direct references to sensitive operations (clock reads, RNG, crypto primitives, filesystem, stderr, sqlite handles) that bypass the checked helpers. A hit fails the build with the offending file and line. The audit is the broad helper-bypass tripwire - it watches every module implementation, including internal helpers.

`zig build test-module-governance` is the public built-in governance gate. It runs `zigts expert verify-modules --builtins --strict --json` against the authoritative built-in set from `packages/zigts/src/builtin_modules.zig` and fails on:

- direct forbidden effect usage
- undeclared helper-capability use
- binding/spec specifier drift
- binding/spec capability drift
- missing spec artifacts for public built-ins

Run it after any change under `modules/`:

```bash
zig build test-capability-audit
zig build test-module-governance
```

The release CI job runs both checks explicitly. Local `zig build test` also depends on `test-module-governance`.

## Adding a new module

Follow these steps, in order:

1. **Declare the `ModuleBinding`** in your new `modules/foo.zig` with `specifier`, `effect` (`.read`, `.write`, or `.none`), and `required_capabilities`. Capability list must be exact - list nothing you do not call, and list everything you do call.
2. **Wire the exports** into `modules/root.zig` so the resolver picks them up.
3. **Call guarded helpers only through `module_binding` wrappers.** If you need a direct call inside a helper function, push and pop the active context manually (see the patterns referenced above).
4. **Run the audits**: `zig build test-capability-audit test-module-governance` must pass.
5. **Run the full test matrix**: `zig build test test-zigts test-zruntime`.
6. **Add a fixture** under `tests/validate/` and, if the module has a handler-visible surface, an example under `examples/` with a `.test.jsonl` wired into `scripts/test-examples.sh`.
7. **Update this document** with the new row in the capability table.

If the module needs a capability not yet in the enum, you are extending the governance surface itself: add the variant to `ModuleCapability`, teach the audit pass to recognize the new guarded operations, and update the table above. A new capability is a security-relevant change and should go through a CODEOWNERS review.

## Cross references

- [`packages/zigts/src/module_binding.zig`](../packages/zigts/src/module_binding.zig) - enum, wrappers, thread-local context, `requireCapability`.
- [`packages/zigts/src/modules/resolver.zig`](../packages/zigts/src/modules/resolver.zig) - where `wrapNativeFnWithCapabilities` is applied per exported function.
- [`SECURITY.md`](../SECURITY.md) - reporting and scope.
- [`docs/threat-model.md`](threat-model.md) - threat model for the rule review system (complementary to this doc's module boundary story).
- [`docs/verification.md`](verification.md) - the handler-level verification pass that lives alongside capability enforcement.
