# Roadmap

What is deliberately deferred from the current beta and what comes next. This is
the single forward-looking document; shipped changes live in
[CHANGELOG.md](../CHANGELOG.md), and the current command and feature surface is
described in the [User Guide](user-guide.md).

## Current release: v0.1.0-beta

The beta ships a threaded HTTP/1.1 server with per-request runtime isolation, the
two-pass zigts engine with tiered JIT, the restricted JS/TS subset, 22 virtual
modules, the compile-time analyzers (contract extraction, type and flow checking,
fault coverage, system linking), default-on Ed25519 attestation, local deploy, and
durable workflows. See the [User Guide](user-guide.md) for the full surface and
[docs/internals/architecture.md](internals/architecture.md) for the design.

## Deferred

These are known, intentional gaps in the beta. Each is tracked for a later release.

**Hosted cloud deploy.** `zigttp deploy --cloud` and the account commands
(`login`, `logout`, `review`, `grants`, `revoke-grant`) are gated at the CLI
boundary and reject with a "not in this beta" message. The supported path is the
self-contained binary `zigttp deploy` produces, which runs on any host, container,
or FaaS target. The control-plane code stays in the tree, ready to re-enable once
the path has CI smoke coverage.

**Evented I/O backend.** Zig 0.16's `std.Io.Evented` networking surface is
incomplete (Dispatch and Uring return `error.NetworkDown`, Kqueue panics). The
runtime ships a threaded-only HTTP server on every platform. Revisit when the Zig
stdlib networking matures.

**Multipart parser.** Handlers receive raw bodies; `multipart/form-data` is not
parsed in the runtime. Handlers that need it must parse it themselves, respecting
quoted boundaries. Tracked for a future virtual module.

**JIT code-cache eviction.** Compiled native code is allocated per
`CompiledFunction` and freed on handler swap. There is no size-bounded LRU. This is
safe for short-lived FaaS processes; a future release adds eviction for
long-running servers.

**Tier-2 JIT stability.** The optimized, loop-specialized code paths exist
(`packages/zigts/src/jit/optimized.zig`) and pass the test suite. Treat them as
opt-in for performance characterization, not as a stability guarantee at v0.1.0.

**WebSocket peer-sent close-code validation.** RFC 6455 frame parsing is correct;
explicit validation of peer close codes is deferred.

**HandlerPool init leak under allocation failure.** `Runtime.installHttpConstructors`
and the structurally similar `install*` siblings in `zruntime.zig` orphan a
just-created prototype object when a midway `addDynamicMethod` returns
`OutOfMemory`. A regression test is wired but skipped; the structural fix is slated
for v0.2.0.

## Supported platforms

- **macOS** (Darwin) on Apple Silicon and Intel.
- **Linux** on x86_64 and arm64 (`ubuntu-latest` in CI).
- **Windows**: not supported in v0.1.0.
- **Zig toolchain**: 0.16.0 stable. `build.zig.zon` declares
  `minimum_zig_version = "0.16.0"`. Pre-release nightlies are not supported.

## Compiled out by default

The browser proof workbench (`zigttp studio`) and the in-process edge runtime
(`zigttp edge`) are opt-in build flags, not deferred features. Build with
`zig build -Dstudio` or `zig build -Dedge` to compile them in. Without the flag,
each command prints a one-line rebuild hint and exits non-zero.
