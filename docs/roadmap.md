# Roadmap

The roadmap is the only forward-looking document in the maintained docs. It
records the current support boundary and work outside the current
implementation. Shipped changes live in `../CHANGELOG.md`; current user
behavior lives in [User Guide](user-guide.md).

## Supported Now

- macOS and Linux on x86_64 and aarch64.
- Zig `0.16.0` as declared by `build.zig.zon`.
- Threaded HTTP/1.1 server with per-request runtime isolation.
- Restricted JS/TS/TSX handler execution through `zigts`.
- The five core `zigttp` commands: `init`, `dev`, `test`, `expert`, `deploy`.
- Local self-contained deploy artifacts with default-on attestation.
- Compile-time checks for response paths, Result and optional handling,
  state isolation, active specs, flow properties, contracts, and module policy.
- 22 built-in `zigttp:*` virtual modules listed in
  [Virtual Modules](virtual-modules/README.md).
- Optional Studio and edge runtime builds via `-Dstudio` and `-Dedge`.

## Current Limitations

- Hosted cloud deploy is not part of the current CLI surface. `deploy` builds a
  local binary; account, grant, remote review, and provider-control-plane flows
  are not documented as supported user workflows.
- The runtime uses the threaded HTTP server path. The evented `std.Io`
  networking path is not a supported request backend.
- Handlers receive raw `multipart/form-data` bodies. Parse multipart bodies in
  handler code or behind a virtual module once one exists.
- Compiled native code is freed on handler swap, but there is no size-bounded
  JIT cache eviction policy for long-running processes.
- WebSocket frame handling is implemented, but peer-sent close bodies are not
  exposed as parsed close-code metadata.
- Windows is not supported.

## Planned Work

- Keep `zigttp help --all`, `packages/zigts/src/builtin_modules.zig`, and
  `packages/modules/module-specs/` as the sources of truth for CLI and module
  docs.
- Add server-level rate limiting only if the standalone server becomes a
  first-class unproxied deployment target; application limits are currently
  handled with `zigttp:ratelimit`.
- Add size-bounded native JIT code eviction and long-running-process tuning
  once production usage needs it.
- Promote hosted deploy only after the control-plane path has CI smoke coverage
  and user-facing commands are present in default docs.
