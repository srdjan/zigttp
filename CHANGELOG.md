# Changelog

Changes to zigttp. Format: [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). SemVer kicks in at 1.0; until then, minor bumps may include breaking changes (called out in the release notes).

For releases prior to v0.16 see git tags and [RELEASE_CHECKLIST.md](RELEASE_CHECKLIST.md).

## [Unreleased]

### Changed

- Contract upgrades now diff every canonical monotonic proof property. Losses
  that were previously omitted can now tighten `zigts prove` and proof-gate
  verdicts according to the existing critical, warning, and informational
  severity policy.
- Expert model selection is now provider-aware across `--model`, `/model`, and
  RPC `model.{list,set}`. Listings show only the active provider, exact model
  IDs cannot switch providers, and rejected startup or in-session choices leave
  the model and request budget unchanged. Anthropic keeps credential precedence
  when both keys are configured. The registry now includes the shipped,
  experimental `gpt-4o-mini` backend with a 128,000-token context window,
  16,384-token output capability, and zigttp's existing 8,192-token request
  policy.

## [0.18.0] - 2026-07-13

Expert-agent convergence release: the `zigttp expert` loop is the sole surface
touched. The engine, runtime, and analyzer contract are unchanged from 0.17.0
(same policy hash and semantics hash).

### Added

- `zigttp ledger stats`: a subcommand that aggregates every persisted
  `session_summary` for the current workspace into the three cross-session
  metrics STRATEGY.md stakes but never measured - expert success rate (fraction
  of sessions that reached a verified proof), median round-trips to first green
  proof, and median proven-path ratio. Reads the last summary row per session
  (so a `--resume-extended` session reflects its final state), skips sessions
  without one, and is best-effort on unreadable files. (#9, #11)

### Changed

- The expert default model is now Claude Sonnet (`claude-sonnet-4-6`), the
  measured 7/7 first-draft-correct baseline, replacing Haiku. Haiku stays
  reachable via `--model` and the `ZIGTTP_CODEGEN_MODEL` harness knob. (#5)
- The expert per-request output-token budget now tracks the active model's
  registry maximum (Sonnet: 64k) instead of the conservative 8192 default, and
  applies to the default model too, not just a `--model` override - removing the
  silent handler-size ceiling where a whole-file `apply_edit` exceeded 8192
  output tokens. A response that still truncates now maps to a recoverable
  `OutputTruncated` ("split the change") instead of the opaque fatal
  `InvalidEditArgs`; a genuinely malformed args object still returns
  `InvalidEditArgs`. (#3)
- The expert now surfaces the compiler's Cost Contracts: `PropertiesSnapshot`
  mirrors the engine's full `HandlerProperties` boolean surface (adds the
  previously missing `cost_bounded` and `post_only`), the proof card highlights
  a `cost_bounded` chip, the `/ledger` proven-path ratio folds in the new
  dimensions (16/16 -> 18/18), and the persona teaches the loop-bound discharge
  forms. A comptime drift gate now asserts the expert mirrors every engine
  boolean property, so a new engine proof dimension fails the build until the
  expert surfaces it. (#6)
- A `tool_result` body is capped at 32 KB before it enters the transcript, with
  a marker pointing the model at a `workspace_read_file` range re-read, so a
  single large output no longer inflates the input token count of every
  subsequent roundtrip. The RPC surface now auto-compacts after each turn
  (mirroring the interactive REPL) and emits a notification when compaction
  fires, so a long-lived IDE session cannot dead-end on a `PromptTooLong` the
  machine client cannot resolve. (#10, #12)
- The OpenAI expert backend is marked experimental in `zigttp auth` help and the
  credentials help (Anthropic is the measured, supported path), and its
  `contextWindowTokens` now returns gpt-4o-mini's true 128k window instead of
  the Anthropic-sized 200k assumption, so a long OpenAI session compacts before
  the real limit rather than dead-ending. Full OpenAI parity remains deferred.
  (#7)
- The expert retry loop is now information-complete: the `.run_veto` arm records
  the model's draft as an `apply_edit` tool_use closed by the compiler verdict
  as a tool_result, and the compiler-authored repair block persists as a
  follow-up system note, so the whole retry context lives in the transcript
  instead of a transient buffer a mid-repair tool call would erase. The per-turn
  wall-clock timeout is raised 60s -> 300s (recorded convergence for complex
  handlers ran 80-96s), and a rolling cache breakpoint on the last message block
  lets the conversation prefix hit the prompt cache each roundtrip. (#1, #2, #4)

## [0.17.0] - 2026-07-11

### Added

- **Cost Contracts:** the compiler proves each handler's input-relative resource complexity as a signed per-route cost envelope. An I/O call inside a `for...of` over a request-derived collection now reports a linear bound (`1 + 1*|ids|`) instead of the previous `max_io_depth` undercount that walked the loop body once. Bounds discharge to a constant by sizing the loop source: a literal array, `range(n)`, `Object.keys` of an object literal, `.slice(0, k)`, a SQL statement ending in `LIMIT n`, or a `zigttp:validate` schema field declaring the newly supported `maxItems` keyword. Surfaced as the `cost_bounded` proof chip and `Spec<"cost_bounded">`, a `contract_diff` cost lane (widening a bound to `unbounded` is breaking; a bounded widening is additive) that feeds `prove`, `prove-behavior`, the expert equivalence receipt, and `proofs gate`, deploy manifest tags (`zigttp:costClass` / `zigttp:costBound` / `zigttp:costSource` / `zigttp:costWorstCase`), and a log-only runtime cost fuse that records a `cost_bounded` soundness incident when a request exceeds its proven envelope. The `costEnvelope` is written to `contract.json` and covered by the `Zigttp-Attest` JWS signature.

### Changed

- **Breaking:** `zigttp:websocket` no longer exports `roomFromPath`. Upgrade handlers now receive the normalized request path as the room key and callbacks receive that room explicitly; migrate callers to the callback `room` argument or `getWebSockets(room)`. This is an intentional beta cutover with no compatibility stub.
- **Breaking:** generic `tools.invoke` RPC access is now limited to in-process analysis and workspace reads. Tools that execute processes, persist agent state, or write the workspace remain available only on their explicitly trusted/model surfaces and are omitted from RPC discovery.
- **Breaking:** durable `run()` no longer trusts an automatic retry or a duplicate-response replay by default. Reusing a completed durable response, or retrying a run that a crash left incomplete, now requires either a workflow proof (`idempotent` for response reuse, `retry_safe` for retries) or a client-supplied `Idempotency-Key` header matching the `run()` key, recorded in an on-disk ledger keyed by that header. Without one of those, the handler gets a soft `599` JSON error (`DurableIdempotencyUnproven` / `DurableRetryUnproven`) instead of silently re-running or replaying a side effect. Enforcement is on by default for any handler with durable storage configured; unproven workflows should either declare `Spec<"idempotent" | "retry_safe">` or have callers send `Idempotency-Key`.
- `zigttp link`/`zigttp rollout`: payload-compatibility, cross-boundary/injection, and failure-cascade/retry-safety checks now also run over HATEOAS affordance links (`resource()`/`follow()`), not just ordinary `fetchSync`/`serviceCall` links. An existing `system.json` whose affordance links were not previously proof-checked may see `zigttp rollout` report `needs_review`/`breaking` after upgrading, with no handler code change.

### Fixed

- Durable crash recovery no longer double-frees nested function bytecode when `recoverOne` replays a run to completion. Every non-closure function value created at runtime (including the zero-upvalue arrow callbacks passed to `run()`/`step()`) shared its underlying bytecode with the enclosing function's constant pool, so both were freed independently on teardown; a durable run resuming after a crash and finishing successfully could corrupt memory. Fixed by deduplicating bytecode teardown against a single per-teardown-pass tracking set.
- Durable-run dead-letter quarantine (`durable dead-runs`) now actually survives a server restart: the standing dead-run check previously lived only inside the scheduler's tracked poll path, so the untracked recovery pass run once at every `zigttp serve --durable` startup re-attempted every incomplete oplog unconditionally, including ones with a standing `quarantined` or operator-`discarded` record. The check now runs unconditionally for every recovery pass.
- `zigttp durable dead-runs replay` no longer reports success when the on-disk record fails to delete; a failed delete now surfaces as an error instead of leaving the run permanently skipped while the operator was told it was cleared.
- `zigttp durable dead-runs list` no longer aborts entirely when one record file is malformed; the corrupt entry is skipped so other quarantined runs still show.
- `zigttp durable --help` now writes to stdout, matching every other help path in the CLI (it previously wrote to stderr).
- A dead-run record write that fails to persist (disk full, permission error) no longer silently un-quarantines the run on the next poll; the retry tracker now tracks whether the write was actually confirmed and retries it instead of treating a missing record as an external `replay`.
- `durable dead-runs replay`/`discard` now serialize against each other (and against the server's own quarantine writes) for the same run id, so two concurrent invocations can no longer interleave and silently reverse each other's reported success.

### Added

- Source-mapped runtime traps: the JS engine now carries a per-function line table (source `offset -> line:column`) from IR through bytecode generation, peephole compaction, and the bytecode cache (format v4), so an interpreter-tier type fault (TypeError/NotCallable) records the original source line of the faulting instruction. The resolved `line:column` is surfaced in the type-fault 500 response body and in the soundness-incident record. JIT-tier source maps, filename threading, and per-response-path proof attribution are follow-ups.
- Proof-explained failures: a runtime handler fault no longer returns a bare `500 Internal Server Error`. The runtime maps the fault to the proof chip that guards its class (a type error to `optional_safe`/`result_safe`, a non-Response return to `exhaustive_returns`) and attributes it against what the handler actually proved: the body names the *unproven* guarding chip as the predicted cause, and a fault on a path where every guarding chip was proven is flagged as a (possible) soundness incident. Covers all three runtime 500 paths (handler-error, pending-exception, non-Response return).
- `--incident-log <FILE>`: opt-in JSONL sink for soundness incidents. Each confirmed incident (a runtime fault on a path the compiler proved safe) is appended as one JSON line (`{kind, method, path, proven, detail}`) through a shared O_APPEND fd, alongside the always-on error log. Off by default.
- `--workflow-queue`: recovery for `.reclaim-*` files left behind by a crashed lease-reclaim attempt (a stray reclaim is now surfaced via `zigttp` queue tooling and reclaimed automatically once it is older than the lease window, instead of being invisible to future claims).
- `--workflow-queue`: a dead-lettered child request is now resolvable via `zigttp proof replay`/queue replay instead of returning a terminal error to the parent durable step.
- Durable fetch (`zigttp:fetch`'s `fetch()`) now stops its retry/backoff loop as soon as the enclosing step's deadline passes, instead of continuing to retry past it.
- `--workflow-queue`: lease-reclaim retry time is now jittered so items whose leases expire around the same wall-clock moment don't all become re-eligible in the same scheduler tick.
- `durable dead-runs list|show|replay|discard`: operator CLI for durable-run dead-letter records, mirroring `workflow-queue`'s existing dead-letter surface for a permanently-failed crash-recovery run instead of queued child dispatch.
- `ZTS509`: fails the build when `workflow.call`/`saga`/`fanout`/`follow` is used inside a `durable.step()` callback instead of silently losing durability at runtime.
- `ZTS510`: proves that a statically-constructed `saga([...])` has a `compensate` on every step except possibly the last; dynamically-constructed sagas remain unproven.

## [0.1.1-beta] - 2026-06-29

### Added

- `zigttp expert`: the agent asks one clarifying question when a request is materially ambiguous (which auth scheme, which route, what "safe" means) instead of guessing and spending its veto/round-trip budget on a misread. Implemented as a persona rule in `packages/pi/src/expert_persona.zig`: the question ends the turn as plain text and the answer continues on the next line, reusing the existing multi-turn transcript (no turn-machine change).
- `zigttp expert`: per-session metrics. Each session writes a `session_summary` event to `events.jsonl` at close with turn count, model round-trips, verified-edit count, round-trips to first green proof, and a proven-path ratio (discharged proof guarantees over those tracked, read from the final verified handler's `PropertiesSnapshot`). `/ledger` prints the live numbers mid-session. Metrics key on the `verified_patch` signal, so a plain-text clarifying turn counts as a turn without being mistaken for an applied edit.
- `zigttp:queue`: opt-in actor-style messaging (`send`, `request`, `receive`, `ack`, `nack`, `reply`) backed by a new in-memory `ActorQueue` with per-actor priority mailboxes, lease-based redelivery, and dead-lettering after max attempts. Wired through `RuntimeConfig`/server/`runtime_cli` behind `--actor-queue`.
- `--workflow-queue`: opt-in persisted durable dispatch queue for `zigttp:workflow` call/follow/fanout child requests. Child requests are persisted under the durable workflow queue, leased while running, and completed response parts are written before the parent durable step result is recorded. Requires durable storage and a system bundle; saga dispatch remains unsupported in queue mode since its closure-based dispatch does not fit the flat queued child boundary.

## [0.1.0-beta] - 2026-06-23

### Added

- v0.1.0-beta polish slice: capability-denial unit tests in `packages/zigts/src/module_binding.zig` pin both halves of policy gating (the `ctx.capability_policy.allowsXxx` check and the active-module `.policy_check` declaration) across env, cache, sql, sql-write, and egress; deterministic HTTP-parser fuzz harness in `packages/runtime/src/http_parser.zig` covers parseRequestLine, parseQueryString, parseContentLengthValue, parseContentLength, and findHeaderEnd across 16k+ seeded iterations and pins regression behavior for CR/LF preservation and `..` path traversal preservation; studio JSON/ndjson responses are bounded at 1 MiB via the new `finishStudioOwnedResponse` helper, returning 413 on overflow instead of unbounded `setBodyOwned`.
- `docs/contracts-and-sandboxing.md`: explicit per-request vs startup vs hot-swap vs build-time enforcement story, including the `routes_dynamic = true` fall-through and the hot-swap pool-policy transition.
- `docs/roadmap.md`: single forward-looking doc naming hosted cloud deploy, the evented I/O backend, runtime multipart parsing, and JIT code-cache eviction as explicit non-goals for v0.1.0.
- `precompile`: `--build-time` and `--git-commit` CLI flags. `build.zig` now captures the short git SHA at configure time and passes it through, so `comptime(__BUILD_TIME__)` and `comptime(__GIT_COMMIT__)` substitute real values (ISO-8601 UTC timestamp and 12-char SHA) instead of `undefined`. Falls back to the wall clock and the sentinel `"unknown"` when neither flag nor git is available.
- `SECURITY.md` / `docs/threat-model.md`: explicit statement that only bytecode passing `bytecode_verifier.zig` is dispatched into the VM, and a Known Footguns section calling out that `multipart/form-data` parsing is handler-side, not runtime-side.
- `CLAUDE.md` virtual-modules section: corrected to describe the two-tier registry that already exists (`packages/modules/src/root.zig::catalog` + `all_bindings` for SDK-pure modules; `packages/zigts/src/builtin_modules.zig::builtins` for the engine-coupled superset). Previous wording wrongly implied bindings were declared in `module_binding.zig`.
- Witness corpus: every flow-property counterexample the compiler discovers persists to `.zigttp/witnesses/<short_hash>/<key>.witness.jsonl` so a falsifying input does not need to be rediscovered next session. Auto-population runs in `precompile.runCheckOnly` so `zigts check`, `zig build -Dhandler=...`, `--watch --prove`, and the agent tools (`pi_repair_plan`, `pi_goal_check`) all populate the corpus on every analysis pass. The format reuses `counterexample.writeJsonl` exactly so persisted witnesses feed the runtime witness-replay path without translation; inspect and manage them with `zigttp witnesses`.
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
  - _Correction (as of 0.18.0): only the `deploy --cloud` flag-level rejection remains. The account verbs (`login`/`logout`/`review`/`grants`/`revoke-grant`) were removed entirely and now hit the generic unknown-command path, and `packages/runtime/src/deploy/` no longer exists._
- The experimental `assert-intent` command is hidden from `zigttp help --all`; it still dispatches for callers that invoke it directly.
  - _Correction (as of 0.18.0): `assert-intent` no longer dispatches at all; a guard in `cli_help.zig` keeps it out of `help --all`. Intent assertions are extracted into the contract at build time (`-Dcontract`), not via a command._
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

[Unreleased]: https://github.com/srdjan/zigttp/compare/v0.18.0...HEAD
[0.18.0]: https://github.com/srdjan/zigttp/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/srdjan/zigttp/compare/v0.1.1-beta...v0.17.0
[0.1.1-beta]: https://github.com/srdjan/zigttp/compare/v0.1.0-beta...v0.1.1-beta
[0.1.0-beta]: https://github.com/srdjan/zigttp/compare/v0.16.0-rc2...v0.1.0-beta
[0.16.0-rc2]: https://github.com/srdjan/zigttp/compare/v0.16.0-rc1...v0.16.0-rc2
[0.16.0-rc1]: https://github.com/srdjan/zigttp/compare/v0.15.0...v0.16.0-rc1
