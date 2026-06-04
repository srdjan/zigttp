# Changelog

Changes to zigttp. Format: [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). SemVer kicks in at 1.0; until then, minor bumps may include breaking changes (called out in the release notes).

For releases prior to v0.16 see git tags and [RELEASE_CHECKLIST.md](RELEASE_CHECKLIST.md).

## [Unreleased]

### Added

- v0.1.0-beta polish slice: capability-denial unit tests in `packages/zigts/src/module_binding.zig` pin both halves of policy gating (the `ctx.capability_policy.allowsXxx` check and the active-module `.policy_check` declaration) across env, cache, sql, sql-write, and egress; deterministic HTTP-parser fuzz harness in `packages/runtime/src/http_parser.zig` covers parseRequestLine, parseQueryString, parseContentLengthValue, parseContentLength, and findHeaderEnd across 16k+ seeded iterations and pins regression behavior for CR/LF preservation and `..` path traversal preservation; studio JSON/ndjson responses are bounded at 1 MiB via the new `finishStudioOwnedResponse` helper, returning 413 on overflow instead of unbounded `setBodyOwned`.
- `docs/contracts-and-sandboxing.md`: explicit per-request vs startup vs hot-swap vs build-time enforcement story, including the `routes_dynamic = true` fall-through and the hot-swap pool-policy transition.
- `docs/roadmap.md`: single forward-looking doc naming hosted cloud deploy, the evented I/O backend, runtime multipart parsing, and JIT code-cache eviction as explicit non-goals for v0.1.0.
- `precompile`: `--build-time` and `--git-commit` CLI flags. `build.zig` now captures the short git SHA at configure time and passes it through, so `comptime(__BUILD_TIME__)` and `comptime(__GIT_COMMIT__)` substitute real values (ISO-8601 UTC timestamp and 12-char SHA) instead of `undefined`. Falls back to the wall clock and the sentinel `"unknown"` when neither flag nor git is available.
- `SECURITY.md` / `docs/threat-model.md`: explicit statement that only bytecode passing `bytecode_verifier.zig` is dispatched into the VM, and a Known Footguns section calling out that `multipart/form-data` parsing is handler-side, not runtime-side.
- `CLAUDE.md` virtual-modules section: corrected to describe the two-tier registry that already exists (`packages/modules/src/root.zig::catalog` + `all_bindings` for SDK-pure modules; `packages/zigts/src/builtin_modules.zig::builtins` for the engine-coupled superset). Previous wording wrongly implied bindings were declared in `module_binding.zig`.
- Witness corpus: every flow-property counterexample the compiler discovers persists to `.zigttp/witnesses/<short_hash>/<key>.witness.jsonl` so a falsifying input does not need to be rediscovered next session. Auto-population runs in `precompile.runCheckOnly` so `zigts check`, `zig build -Dhandler=...`, `--watch --prove`, and the agent tools (`pi_repair_plan`, `pi_goal_check`) all populate the corpus on every analysis pass. The format reuses `counterexample.writeJsonl` exactly so a persisted witness can be replayed via `zigttp mock --replay` without translation.
- `zigttp witnesses` CLI: `list`, `pin`, `unpin`, `prune`, and `synthesize` subcommands manage the corpus. `list` with no handler argument summarises every corpus directory discovered under `.zigttp/witnesses/`. `synthesize <handler> <spec>` seeds a structural witness for one of the cause-only specs (`deterministic`, `read_only`, `retry_safe`, `idempotent`, `state_isolated`, `fault_covered`) using the per-property `Try:` suggestion from `spec_discharge.suggestionFor`.
- `pi_witnesses` agent tool plus `/witnesses <path>` slash command: returns total + per-property counts + first 20 entries with property, summary, and pinned status. The expert persona's new "Witness corpus awareness" block steers the agent to consult coverage before drafting repair (Specs with zero witnesses are unprobed; pinned witnesses are load-bearing).
- `zigts check --json` proof envelope grows a `proof.witnesses` block (total + by_property breakdown) read from the on-disk corpus per check.
- Proof studio gains a Witnesses tile in the right pane: total, per-property counts, and the first 20 entries with key prefix, summary, and pinned status. Heading hides itself on empty corpora.
- `--watch --prove` HUD toast: when a build re-fires a previously pinned witness, `live_reload` prints `[witnesses] N pinned witness(es) re-fired` so authors notice regressions of patterns they explicitly chose to defend.
- Default `Spec<...>` declarations on `examples/handler/handler.ts` and `examples/system/users.ts` showing the proof-first authoring style. Both discharge cleanly against today's classifier.
- `zigts expert`: full token accounting per session - input, cache-read, cache-write, and output tokens tracked cumulatively and displayed after each model turn as `[tokens: in=N cache_r=N cache_w=N out=N]`.
- `zigts expert`: session compaction via `/compact` - collapses the current transcript into a single system note; the session continues from the summary.
- `zigts expert`: session branching via `/fork` and `--fork <session-id>` - opens a new session that continues from the end of an existing session's transcript.
- `zigts expert`: `/tree` command lists all sessions for the current workspace with creation timestamps.
- `zigts expert`: `--continue` flag as an alias for `--resume`.
- `zigts expert`: `--tools minimal|full` flag selects the tool preset at launch. `minimal` loads only workspace read/list/search; `full` loads the complete compiler tool set (default).
- `zigts expert`: `/model` shows the active model and lists available IDs; `/model <id>` switches mid-session. Unknown IDs print an error.
- `zigts expert`: skills catalog with `/skills` and `/skill:<name>` - lists baked-in skill shortcuts and sends the selected skill body as a model prompt.
- `zigts expert`: prompt template catalog with `/templates` and `/template:<name> [args...]` - lists baked-in templates and expands positional args (`{{1}}`, `{{2}}`, `{{args}}`) before sending.
- `zigts expert`: `/settings`, `/hotkeys`, and `/changelog` informational commands.
- `zigts expert`: Route Forge v1 with `/feature route ...` preview, `/forge route ...` compiler-proven route candidates, and explicit REPL apply through the existing compiler veto.
- Author-declared spec obligations via the built-in `Spec<...>` generic alias from `zigttp:types`. Authors write `import type { Spec } from "zigttp:types"`, alias the obligations as a normal TS type (e.g. `type Guardrails = Spec<"idempotent" | "deterministic">`), and intersect them on the handler return type (`Response & Guardrails`). The verifier discharges declared specs against `HandlerProperties` and emits ZTS500 (not_discharged), ZTS501 (incompatible_with_import), or ZTS502 (unknown_name). The proof HUD, proof ledger, `zigts check --json`, and `pi_specs_status` agent tool all expose the active set.
- TypeScript intersection types (`A & B`) in the type pool: parser support, instantiation substitution, structural assignability, and round-trip resolution through `TypeEnv` aliases. Prerequisite for the `Spec<...>` annotation form; also unblocks future intersection-typed primitives.
- `zigts expert`: `pi_specs_status` tool returns the active spec set declared on the handler return type plus each spec's discharge state. The persona's "Spec-driven repair" block routes spec questions through this tool rather than the `--goal` flag.
- Root-level `LICENSE` (MIT), `SECURITY.md`, `CONTRIBUTING.md`, and `CODEOWNERS`.
- This `CHANGELOG.md`.
- `docs/internals/capabilities.md` - complete capability enforcement map: every `ModuleCapability` variant, which virtual modules declare it, and the enforcement helpers (`pushActiveModuleContext`, `requireCapability`, `wrapNativeFnWithCapabilities`) that gate it. Explains the thread-local context model that makes capability enforcement compose with `HandlerPool`.
- `packages/runtime/src/handler_loader.zig` - shared loader that collapses the duplicated `HandlerSource` switch in `replay_runner.zig`, `test_runner.zig`, and `durable_recovery.zig` into a single `load(allocator, handler, label)` helper, plus inline tests covering `inline_code`, `embedded_bytecode`, and `appended_payload` branches.
- Linux (`ubuntu-latest`) entry in the CI test matrix. `.github/workflows/release.yml` now runs `zig build test test-zigts test-zruntime`, the release build, `scripts/test-examples.sh`, the policy-hash check, and the expert subsystem verification on both macOS and Linux.

### Changed

- `zigttp expert` and `zigts expert` now fail fast when no model backend is configured. With neither `ANTHROPIC_API_KEY` nor `OPENAI_API_KEY` set to a non-empty value, the CLI prints setup guidance and exits non-zero instead of launching the agent UI on the offline stub. This is a v1 semantic change to the contract published in `docs/internals/zigts-expert-contract.md`; the prior stub fallback for an unset environment is no longer reachable from either CLI, including the `--print --mode json` event stream.
- Hosted cloud deploy is deferred from v0.1.0-beta. `zigttp deploy --cloud`, `login`, `logout`, `review`, `grants`, and `revoke-grant` are gated at the CLI boundary and reject with a "not in this beta" message; they are dropped from `zigttp help --all`. The control-plane code stays in `packages/runtime/src/deploy/`, ready to re-enable once the path has CI smoke coverage.
- The experimental `assert-intent` command is hidden from `zigttp help --all`; it still dispatches for callers that invoke it directly.
- `AGENTS.md` documentation index now covers the full `docs/` directory.
- `scripts/test-examples.sh` runs `zig build` up-front so CI fails fast on a broken runtime rather than emitting N cryptic per-suite errors.
- `replay_runner.zig`, `test_runner.zig`, and `durable_recovery.zig` delegate handler-source loading to `handler_loader.zig` instead of each carrying its own switch.

### Fixed

- `Runtime.installHttpConstructors` (and the structurally similar `install*` siblings in `zruntime.zig`) no longer leak the just-created prototype/constructor JSObject when a midway `addDynamicMethod` returns `OutOfMemory`. Each object now carries an `_unowned` flag with an `errdefer ...destroyBuiltin(...)` guard until it is appended to `ctx.builtin_objects`, so a mid-init allocation failure frees the orphan instead of leaking it. The previously `SkipZigTest`'d `HandlerPool init under FailingAllocator never leaks` test is now active and sweeps the regression fail points.
- Removed an unsafe `catch unreachable` on the outbound `fetch` hot path in `zruntime.zig`; malformed URI hosts now return a typed `InvalidUrl` fetch error instead of panicking the worker.
- `builtins/math.zig:prng` is now `threadlocal`. The `Math.random` PRNG state was shared across all pool workers with no synchronization; each worker now owns its own stream. Trace recording and replay semantics are unchanged.
- `builtins/json.zig:json_shape_cache` is now `threadlocal`. The comment already called it "Thread-local JSON shape cache" but the declaration had drifted to a plain global. Per-thread is what the comment promised and what `clearJsonShapeCache` (called during per-context setup) assumed.
- `interpreter.zig:call_trace_count` is now `threadlocal`. This is a debug-only counter (`ZTS_CALL_TRACE`) that gates how many call-trace lines get printed; as a global it could drop increments under concurrent traced calls. The semantics are now a per-worker print budget instead of a global one - acceptable because the feature is opt-in diagnostic output. The remaining env-var caches at `interpreter.zig:22-50` are unchanged: their races are benign same-value writes.

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
- Step-by-step deploy tutorial (now part of `docs/user-guide.md`).

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
