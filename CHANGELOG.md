# Changelog

All notable changes to zigttp are recorded here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project adheres to Semantic Versioning once it ships 1.0. Until then, minor version bumps may include breaking changes; the release notes will call them out.

Prior to v0.16 the project had no CHANGELOG. For historical releases (v0.7 through v0.15) consult `git log` between tags and the checklist entries in [RELEASE_CHECKLIST.md](RELEASE_CHECKLIST.md).

## [Unreleased]

### Added

- Root-level `LICENSE` (MIT), `SECURITY.md`, `CONTRIBUTING.md`, and `CODEOWNERS`.
- This `CHANGELOG.md`.
- `docs/capabilities.md` - complete capability enforcement map: every `ModuleCapability` variant, which virtual modules declare it, and the enforcement helpers (`pushActiveModuleContext`, `requireCapability`, `wrapNativeFnWithCapabilities`) that gate it. Explains the thread-local context model that makes capability enforcement compose with `HandlerPool`.
- `docs/control-plane-contract.md` - HTTP wire contract for `zigttp deploy` and `zigttp login`: endpoints (`/v1/auth/device/{start,poll}`, `/v1/auth/token/verify`, `/v1/deploy/session`), request/response schemas, status code semantics, credentials file format, cross-compile pipeline, OCI proven-fact labels, Northflank provider endpoints, drift detection, and the minimum contract a self-hosted control plane must implement under `ZIGTTP_CONTROL_PLANE_URL`.
- `packages/runtime/src/handler_loader.zig` - shared loader that collapses the duplicated `HandlerSource` switch in `replay_runner.zig`, `test_runner.zig`, and `durable_recovery.zig` into a single `load(allocator, handler, label)` helper, plus inline tests covering `inline_code`, `embedded_bytecode`, and `appended_payload` branches.
- Linux (`ubuntu-latest`) entry in the CI test matrix. `.github/workflows/release.yml` now runs `zig build test test-zigts test-zruntime`, the release build, `scripts/test-examples.sh`, the policy-hash check, and the expert subsystem verification on both macOS and Linux.
- `examples/url-shortener/` wired into `scripts/test-examples.sh` (currently disabled behind a comment — the fixture has drifted from the handler and is tracked for repair).

### Changed

- `docs/rollout.md` reframed as a historical appendix for the v0.14-v0.15 rule review rollout; active changes now land here.
- `AGENTS.md` documentation index now covers the full `docs/` directory.
- `scripts/test-examples.sh` runs `zig build` up-front so CI fails fast on a broken runtime rather than emitting N cryptic per-suite errors.
- `replay_runner.zig`, `test_runner.zig`, and `durable_recovery.zig` delegate handler-source loading to `handler_loader.zig` instead of each carrying its own switch.

### Fixed

- Removed an unsafe `catch unreachable` on the outbound `fetch` hot path in `zruntime.zig`; malformed URI hosts now return a typed `InvalidUrl` fetch error instead of panicking the worker.
- Three genuinely-racy VM module-level variables are now `threadlocal`: `interpreter.zig:call_trace_count` (debug trace counter incremented on every call), `builtins/math.zig:prng` (`Math.random` state mutated on every use), and `builtins/json.zig:json_shape_cache` (the comment already called it "Thread-local" but the declaration had drifted). The remaining idempotent env-var caches in `interpreter.zig:22-50` are unchanged - their races are benign same-value writes.

## [0.16.0-rc2] - 2026

Post-monorepo restructure. Deploy subsystem matured, scope module added, proven live reload shipped.

### Added

- `zigttp:scope` virtual module for request-scoped lifecycle management with documented contract semantics.
- Proven live reload: `--watch --prove` diffs handler contracts on save and only hot-swaps when the verdict is `safe` / `safe_with_additions`; `--force-swap` overrides.
- Durable admin API and workflow graph extraction for inspecting long-running durable runs.
- Proof-driven response cache for handlers proven `deterministic` + `read_only`; GET/HEAD responses are served from Zig memory with `X-Zigttp-Proof-Cache: hit`.
- `zigttp:service` virtual module for named internal service calls with compile-time payload typing and cross-handler contract proofs.
- Self-extracting binaries parse the embedded contract at startup for env-var validation and route pre-filtering.
- `zigts` type system enhancements (phases 0-6): generic aliases, structural object matching, discriminated union narrowing, type guard functions, distinct types, readonly, template literal types.
- `zigts expert` CLI namespace plus PreToolUse / PostToolUse / SessionStart hook scripts for Claude Code integration.
- Rule review system with diff-aware analysis and CI policy-hash assertion.
- Build-time capability audit: every virtual module implementation must route through the checked helpers in `module_binding.zig`.
- `zigttp deploy` UX: content-addressed OCI refs, env-var config surface, region/wait flags, session refresh, progress output; `zigttp login` supports both interactive token prompt and stdin modes.
- `zigttp:url` and parallel/race I/O primitives (`zigttp:io.parallel`, `zigttp:io.race`).
- Step-by-step deploy tutorial (`docs/deploy-tutorial.md`).

### Changed

- Repository restructured into a Zig monorepo under `packages/` (`runtime`, `zigts`, `tools`, `zigttp-sdk`, `zigttp-ext-demo`).
- `zigts` module lookups are now scoped by specifier so `zigttp-ext` bindings resolve cleanly.
- `--durable-admin` embedded subcommand removed in favor of the standalone `zigttp-admin` binary.
- Deploy subsystem refactored: extracted `json_util`, `threadedIo`, `stderrLine`; removed dead `plan.zig`, `redact`, `render_adapter`, and unused AWS manifest path.
- Docs aligned with restructure: stale paths fixed in `docs/architecture.md`; `switch` references removed across all docs (the JS subset has no `switch`).

### Fixed

- CI: run tests on macOS only (Linux CI remains open; see P1.1 in the project assessment plan), opt into Node.js 24 for GitHub Actions, consolidate test steps and add job timeouts.
- Parser: removed an unreachable branch after for-of handling.
- Stripper / type checker: collapsed duplicated fallback paths; added coverage for the exported distinct-type path.

## [0.16.0-rc1] - 2026

### Fixed

- CI: run tests on macOS only and consolidate test steps with explicit job timeouts after intermittent runner failures on the previous matrix.

## [0.15.0] and earlier

See git tags and `RELEASE_CHECKLIST.md` for the record of shipped items. Known-issue notes from that era include:

- Closure data freed as `BytecodeFunctionData` (fixed via destroyFull closure discriminator).
- Chunked transfer encoding rejected with 501 rather than mis-parsed.
- Bytecode cache validated after deserialization instead of trusted.
- Static file path traversal via symlinks blocked with check-before-open + `follow_symlinks=false`.
- HandlerPool test flake under the build runner (root cause was the closure destroyFull bug above).

[Unreleased]: https://github.com/srdjan/zigttp/compare/v0.16.0-rc2...HEAD
[0.16.0-rc2]: https://github.com/srdjan/zigttp/compare/v0.16.0-rc1...v0.16.0-rc2
[0.16.0-rc1]: https://github.com/srdjan/zigttp/compare/v0.15.0...v0.16.0-rc1
