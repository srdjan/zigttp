# Roadmap

The roadmap is the only forward-looking document in the maintained docs. It
records the current support boundary and work outside the current
implementation. Shipped changes live in `../CHANGELOG.md`; current user
behavior lives in [User Guide](user-guide.md).

## Supported Now

- macOS and Linux on x86_64 and aarch64.
- Zig `0.16.0` as declared by `build.zig.zon`.
- Threaded HTTP/1.1 server with per-request runtime isolation and decoded
  `Content-Length` or `Transfer-Encoding: chunked` request bodies.
- Restricted JS/TS/TSX handler execution through `zts`.
- WebSocket gateway support with parsed peer close-code metadata.
- The five core `zttp` commands: `init`, `dev`, `test`, `expert`, `deploy`.
- Local self-contained deploy artifacts with default-on attestation.
- Compile-time checks for response paths, Result and optional handling,
  state isolation, active specs, flow properties, contracts, and module policy.
- Built-in `zttp:*` virtual modules listed in
  [Virtual Modules](virtual-modules/README.md).
- Optional Studio and edge runtime builds via `-Dstudio` and `-Dedge`.

## Current Limitations

- Hosted cloud deploy is not part of the current CLI surface. `deploy` builds a
  local binary; account, grant, remote review, and provider-control-plane flows
  are not documented as supported user workflows.
- The runtime uses the threaded HTTP server path. The evented `std.Io`
  networking path is not a supported request backend.
- Handlers receive raw `multipart/form-data` bodies from the HTTP server. Use
  `decodeFormMultipart` from `zttp:decode` or handler-owned parsing when a
  handler accepts multipart input.
- Native JIT code has a per-runtime-context soft cap; when the cap is exceeded
  at a JIT compile safe point, compiled function pointers are cleared and the
  context's native code pages are released.
- Windows is not supported.

## Planned Work

- Close the remaining runtime lifecycle verification gaps before hosted deploy
  claims: broader accept-path coverage for deadlines, graceful shutdown, probes,
  and panic isolation; hosted request-timeout policy; and shutdown
  thread-safety semantics.
- Finish the engine<->runtime boundary refactor by routing runtime calls through
  a strict facade, exposing stable runtime-facing engine types, and splitting
  `zruntime.zig` by concern without changing ownership semantics.
- Keep near-term module work limited to table-stakes gaps: fetch resilience,
  capability surfacing, and build-feature diagnostics. Cloud-adapter modules
  stay in a separate evaluated track.
- Keep `zttp help --all`, `packages/zts/src/builtin_modules.zig`, and
  `packages/modules/module-specs/` as the sources of truth for CLI and module
  docs.
- Add server-level rate limiting only if the standalone server becomes a
  first-class unproxied deployment target; application limits are currently
  handled with `zttp:ratelimit`.
- Promote hosted deploy only after the control-plane path has CI smoke coverage
  and user-facing commands are present in default docs.
- Defer VM-loop dedupe until the FaaS hardening, engine facade, and measurement
  gates are stable. The standalone plan lives in
  [Deferred VM Loop Dedupe Plan](../DEFERRED_VM_LOOP_DEDUPE_PLAN.md).
