# Capability Matrix Audit

Snapshot of every first-party `ModuleBinding` in `packages/modules/src/`, comparing the declared `required_capabilities` against what the implementation actually touches. Capability set is defined in `packages/zigts/src/module_binding.zig` (`ModuleCapability` enum: env, clock, random, crypto, stderr, runtime_callback, sqlite, filesystem, network, policy_check, websocket).

## Enforcement model

The capability list is mostly declarative metadata. Only `.runtime_callback` is checked at call time inside module code (see `requireCapability(handle, .runtime_callback)` in `fetch.zig:49`, `service.zig:94`, `websocket.zig:49`). The remaining capabilities feed `effect_inference.zig:429`, which folds them into the per-handler contract's capability set. The runtime/policy layer is responsible for refusing to mount a handler whose contract claims capabilities the deployment policy does not grant.

This means a binding that under-declares its capabilities silently understates the handler's privilege surface in the contract. Fix forward: declare what the module needs, even if no runtime code currently checks.

## Pure modules (no capabilities, verified)

| Module | Declared | Verified pure? |
|--------|----------|----------------|
| `validate` | (none) | yes - schema-driven JSON validation, no stdlib I/O |
| `decode` | (none) | yes - pure JSON/form/query parsing |
| `time` | (none) | yes - uses `std.time.epoch` types only, never reads the current clock; all four functions take epoch as input arg |
| `text` | (none) | yes - HTML escape, slugify, truncate, mask: pure |
| `router` | (none) | yes - pattern matching |
| `http` | (none) | yes - cookie/CORS/content-type parsing |
| `url` | (none) | yes - URL parse/encode |
| `compose` | `comptime_only` | yes - guard/pipe are comptime decorators |

## Modules with capabilities (verified correct)

| Module | Declared | Notes |
|--------|----------|-------|
| `crypto` | `{crypto}` | sha256, hmacSha256 use `std.crypto`; base64 doesn't strictly need crypto but per-module declaration is correct |
| `auth` | `{crypto, clock}` | JWT verify needs clock for exp, JWT sign needs crypto |
| `env` | `{env, policy_check}` | reads process env via the policy-checked path |
| `id` | `{clock, random}` | UUID v4 needs random; ULID needs clock+random; nanoid needs random |
| `log` | `{clock, stderr}` | timestamps + stderr writes |
| `cache` | `{clock, policy_check}` | TTL handling + namespace policy |
| `sql` | `{sqlite, policy_check}` | sqlite calls |
| `ratelimit` | `{clock}` | window arithmetic on epoch |
| `fetch` | `{network, runtime_callback}` | HTTP via runtime callback |
| `durable` (zigts) | `{runtime_callback}` | all state and timers behind the runtime callback |
| `scope` (zigts) | `{runtime_callback}` | runtime-managed scope tracking |

## Fixes applied

### `service` - was `{filesystem, runtime_callback}`, changed to `{network, filesystem, runtime_callback}`

`serviceCallCallback` in `packages/runtime/src/zruntime.zig:3711` constructs a URL from `base_url + path_pattern + query` and dispatches to `fetchSyncResult(rt, &fetch_args)`. This is an HTTP call. The previous declaration omitted `network` entirely, which understated the privilege surface: a handler could import `zigttp:service` and make outbound network calls without the contract claiming network capability. `filesystem` is retained because the install-time read of `system.json` for the service registry is filesystem I/O at startup.

### `websocket` - was `{clock, runtime_callback, filesystem, policy_check, websocket}`, added `network`

WebSocket `send`, `close`, and the broadcast-style `getWebSockets` dispatch all write to the peer socket via runtime callbacks installed in `packages/runtime/src/zruntime.zig:809`. This is network I/O. `filesystem` is retained because `serializeAttachment` / `deserializeAttachment` can persist hibernation state to disk. The two reasons are now noted as comments alongside the array.

## Caveats

- The audit walks first-party modules under `packages/modules/src/`. Third-party modules under `packages/zigttp-ext-demo/` and any future user-supplied bindings need the same review at registration time.
- Adding `.network` to `service` and `websocket` changes their `capabilityHash` (`module_binding.zig:1103`), which propagates into the policy hash baked into proof receipts. Existing attestations on deployed binaries become invalid until rebuilt; `policy-hash.txt` will need regeneration.
- Module-side `requireCapability` is only used for `.runtime_callback`. Extending enforcement to other capabilities at call time would require either (a) a wrapper in `module_binding_adapter.zig` that walks the declared set, or (b) per-function calls to `requireCapability`. Out of scope for this audit.
