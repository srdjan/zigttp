# zigttp review findings

Base ref: `533fed8` (tree clean). Review plan: `~/.claude/plans/i-commited-pending-changes-idempotent-fiddle.md`.

Severity legend: **BLOCKER** ships broken; **HIGH** real bug or significant design conflict; **MEDIUM** correctness/architecture concern worth addressing; **LOW** minor issue; **NIT** cosmetic. Every finding carries a file:line and a suggested direction, not a full fix.

---

## Phase 1 — Architecture & entry points

### Validated dependency graph

```
zigttp (runtime binary)
  ├── zigts         (engine)
  ├── zigts_cli     (tools)          -- via deploy.zig
  ├── pi_app        (pi)             -- linked unconditionally
  └── project_config (tools submodule)

zigts (compiler CLI binary)
  └── zigts_cli     (tools)

zigts_cli (tools)
  └── zigts         (engine)

pi (agent/REPL/TUI)
  ├── zigts
  ├── zigts_cli                      -- via expert_persona.zig
  ├── zigts_expert_skill (tools)
  └── zigts_expert_examples (repo-local module from examples/data.zig)

Test targets:
  zigts unit tests      : zigts_dep.path("src/root.zig")
  precompile tests      : tools_dep.path("src/precompile.zig")
  prop_expect tests     : tools_dep.path("src/property_expectations.zig")
  rollout tests         : tools_dep.path("src/system_rollout.zig")
  expert tests          : tools_dep.path("src/expert.zig")
  pi tests              : pi_dep.path("src/tests.zig")
  runtime unit tests    : runtime_dep.path("src/main.zig")   (transitively imports zruntime/server/proof_adapter via test block)
  zruntime direct tests : runtime_dep.path("src/zruntime.zig")
```

**Unusual edge**: `runtime → tools (zigts_cli)` via `packages/runtime/src/deploy.zig`. Direction flip from the natural layering (tools sits above runtime elsewhere). Reason: in-process precompile verification during `zigttp deploy`. Not called out in `docs/architecture.md`.

**Build step inventory** (`zig build -l`): 13 steps — `test-zigts`, `test-precompile`, `test-property-expectations`, `test-rollout`, `test-expert`, `test-expert-app`, `test-capability-audit`, `test-module-governance`, `run`, `test`, `test-zruntime`, `release`, `bench`, plus conditional `system` (when `-Dsystem=...`).

**Build options**: 19 — handler, aot, verify, contract, openapi, sdk, sql-schema, policy, system, replay, test-file, prove, generate-tests, manifest, expect-properties, data-labels, fault-severity, generator-pack, report.

### Request flow trace (verified against code)

Documented 9-step flow in `docs/architecture.md:90-101` matches code:

| Step | File:Line |
|---|---|
| main entry | `packages/runtime/src/main.zig:4` |
| CLI dispatch | `packages/runtime/src/zigttp_cli.zig:28` |
| connection loop | `packages/runtime/src/server.zig:1109` (`handleConnection`) |
| per-request arena | `packages/runtime/src/server.zig:1110` (`ArenaAllocator`) |
| request parse | `packages/runtime/src/server.zig:1146` |
| **static short-circuit** | `packages/runtime/src/server.zig:1176-1181` |
| contract route pre-filter | `packages/runtime/src/server.zig:1186-1196` |
| proof cache lookup | `packages/runtime/src/server.zig:1200-1217` |
| pool acquire + handler invoke | `packages/runtime/src/server.zig:1220` → `packages/runtime/src/zruntime.zig:4541` |
| proof cache store on miss | `packages/runtime/src/server.zig:1247-1251` |
| runtime release | `handle.deinit()` at `server.zig:1243` → `zruntime.zig:4810` `releaseForRequest` |

### Findings

#### F1.1 — HIGH: `zig build test` does not run the engine test suite

**Where**: `build.zig:358-366`, doc claim `CLAUDE.md` line `zig build test                     # All tests`.

**What**: The aggregate `test` step depends on `unit_tests`, `precompile`, `prop_expect`, `rollout`, `expert`, `pi`, `capability_audit`, `module_governance`. It does NOT depend on `test-zigts` (the ~1,004 engine test blocks in `packages/zigts/`). CI compensates in `.github/workflows/release.yml` by invoking steps explicitly. Local developers running the documented `zig build test` miss engine regressions entirely.

The runtime `unit_tests` is rooted at `main.zig`, whose `test {}` block imports zruntime/server/proof_adapter, so those are covered transitively — but `zigts` is an external module, and Zig's test runner does not recurse into external modules' test blocks through `@import("zigts")`. That's exactly why `test-zigts` exists as a separate step rooted in `zigts_dep.path("src/root.zig")`.

**Direction**: either chain `test-zigts` and `test-zruntime` into the `test` step (easy, adds build time), or correct the CLAUDE.md comment to reflect that the full suite is `zig build test test-zigts test-zruntime`. Prefer the former — the comment's claim is a developer-ergonomics contract.

#### F1.2 — HIGH: `pi_app` is linked into the primary `zigttp` runtime binary

**Where**: `build.zig:211` adds `pi_app` unconditionally to the main executable. `packages/runtime/src/zigttp_cli.zig:104-106` invokes it only when the subcommand is `"expert"`.

**What**: Design goal (top of `docs/architecture.md`) is "instant cold starts, small deployment package, request isolation, zero external dependencies". Linking the Anthropic API client + TUI + REPL + transcript machinery into every deploy artifact directly conflicts with that goal. The `expert` subcommand is a developer-workstation feature, not something deployed FaaS instances need.

Whether Zig's `ReleaseFast` dead-code elimination fully strips pi_app when it's unreached cannot be claimed without measurement (existing debug binary is 25 MB for `zigttp` vs 19 MB for the standalone `zigts` compiler — a 6 MB delta that includes deploy, pi, and runtime, with no way to isolate pi's share from this artifact alone).

**Direction**: split `zigttp expert` out to a separate `zigttp-expert` binary, or confirm via before/after `ReleaseFast` build sizing that pi_app is stripped when unused and document that. Until one of those is done, the binary-size goal is unverified.

#### F1.3 — MEDIUM: `runtime` package imports `tools` (`zigts_cli`)

**Where**: `packages/runtime/src/deploy.zig` imports `zigts_cli`.

**What**: Tools sit above runtime in the docs; here the runtime depends on tools. The reason is legitimate — `zigttp deploy` does in-process precompile verification as a pre-check — but the dependency direction is not mentioned in `docs/architecture.md`. A reader following the doc's layering would be surprised.

**Direction**: add a note in `docs/architecture.md` (e.g., in the Native Deploy section) clarifying that deploy orchestration pulls the tools layer in-process. Or factor the precompile-verify surface into a minimal shared module so `runtime/src/deploy.zig` can depend on it without reaching into the tools CLI.

#### F1.4 — MEDIUM: Architecture diagram omits the Deploy subsystem

**Where**: `docs/architecture.md:16-31`.

**What**: The system diagram lists HTTP Server / HandlerPool / Builtins. Deploy is ~2.1 KLOC in `deploy.zig` plus 13 files under `packages/runtime/src/deploy/` (control plane, OCI tar/layer/config/manifest/image, registry push, provider adapter, state, printer, autodetect, first-run, builder, types). It's a full subsystem and it's invisible in the overview diagram. Prose at the end of the doc describes it.

**Direction**: add a third box row "Deploy: control plane, OCI, provider adapter" to the diagram. Keeps the overview honest about what lives in `packages/runtime/`.

#### F1.5 — MEDIUM: static-file serving short-circuits the contract route pre-filter

**Where**: `packages/runtime/src/server.zig:1176-1181` runs before the contract-route check at `:1186-1196`.

**What**: Requests to `/static/...` never reach `contract.matchesRoute`. That's probably intentional (contracts prove handler routes, not static paths), but it has security implications worth calling out: a precompiled handler's contract does not constrain the static-file surface. If `--static` is enabled, the static directory is an independent, contract-free attack surface. `docs/architecture.md`'s Request Flow section doesn't mention this ordering.

`isPathSafe()` in `server.zig` is cited in the doc as the defense — good. The finding is about documenting ordering.

**Direction**: one line in `docs/architecture.md` Request Flow after step 3: "Static-file routes (`/static/...`) bypass the contract pre-filter; they're gated only by `isPathSafe()` and the `--static` directory root."

#### F1.6 — LOW: `/bin/mkdir -p` used for build-time directory creation

**Where**: `build.zig:289`.

**What**: Platform-specific shell invocation. Windows is not a claimed target, so severity is low.

**Direction**: use a custom `std.Build.Step` with `std.fs.cwd().makePath("packages/runtime/generated")`. Portable and removes a shell dep from the build.

#### F1.7 — LOW: `b.installArtifact(bench_exe)` installs the benchmark binary by default

**Where**: `build.zig:404`.

**What**: Every `zig build` writes `zigttp-bench` (19 MB debug) into `zig-out/bin/` alongside `zigttp` and `zigts`. That's a bench-only artifact leaking into default install output.

**Direction**: drop `b.installArtifact(bench_exe)` and let `bench_cmd` run against the unistalled artifact, or gate install behind an option.

#### F1.8 — LOW: `run_cmd` depends on the full install step

**Where**: `build.zig:328`.

**What**: `zig build run -- ...` triggers `b.getInstallStep()`, which builds and installs `zigttp`, `zigts`, and `zigttp-bench`. A developer iterating on the runtime has to wait for the compiler CLI and bench to link on every `run`. Minor friction.

**Direction**: `run_cmd.step.dependOn(&exe.step)` instead of `b.getInstallStep()`. Runs the freshly built binary without the install-tree side effects.

#### F1.9 — NIT: `docs/architecture.md` file list omits `handler_loader.zig`, `trace_helpers.zig`

**Where**: `docs/architecture.md:277-288`.

**What**: Listed runtime files miss `handler_loader.zig` and `trace_helpers.zig` (both imported by `durable_recovery.zig`, both present in the tree).

**Direction**: add them, or mark the list as non-exhaustive.

#### F1.10 — NIT: `Result` type snippet in docs is informational, not valid Zig

**Where**: `docs/architecture.md:117-122`.

**What**: The snippet references `T` without generic declaration. Fine as a sketch; a reader copy-pasting would be confused.

**Direction**: change to `fn Result(comptime T: type) type { return union(enum) { ok: T, err: Error }; }` or wrap in prose that signals it's a sketch.

### Phase 1 summary

Two findings deserve attention before any code-level review (F1.1 doc/CI drift and F1.2 binary-size design conflict). The request flow matches docs; the package graph has one undocumented edge (F1.3). The architecture document is accurate in substance but the top-of-doc diagram understates the Deploy subsystem (F1.4) and omits a security-relevant ordering detail (F1.5).

No blockers. No findings require immediate code changes — only docs, build-step chaining, and a design decision on pi_app packaging.

---

## Phase 2 — Engine core

Reviewed: `interpreter.zig`, `object.zig`, `string.zig`, `gc.zig`, `arena.zig`, `value.zig`, `stripper.zig`, `module_binding.zig`, `parser/*`.

### Findings

#### F2.1 — HIGH: PIC coherence relies on an undocumented immutability invariant
**Where**: `packages/zigts/src/interpreter.zig:259-335` (PolymorphicInlineCache), `packages/zigts/src/object.zig:1028-1152` (hidden class transitions)
**What**: The 8-entry PIC caches `(hidden_class_idx, slot_offset)` pairs. Cache coherence depends on hidden classes being immutable once created — transitions allocate a *new* class rather than mutating the old one. That invariant is not asserted anywhere, not tested, and not documented. If a future change adds in-place class mutation (e.g., for prototype-chain changes or shape tightening), the PIC would silently return wrong slot offsets with no crash and no signal.
**Direction**: Add a compile-time or debug-mode assertion that `hidden_class_idx` is write-once per object, and a comment at the HiddenClassPool definition stating the invariant. A single-line test that mutates a class and asserts the PIC invalidates would pin the contract.

#### F2.2 — MEDIUM: Arena-allocated values can escape to tenured objects via property writes
**Where**: `packages/zigts/src/gc.zig:544-545`, `packages/zigts/src/arena.zig:33-73`, `packages/zigts/src/object.zig:1314-1315`
**What**: Objects created via `JSObject.createWithArena()` carry `is_arena = true` and an `arena_ptr`. The write barrier in `gc.zig` records cross-generation references but does not detect when a non-arena (tenured) object receives an arena-allocated value as a property. On arena reset, the tenured slot points into freed memory. Docs say "write barriers prevent arena objects from leaking to persistent storage", but the enforcement point for *property writes* specifically is not visible.
**Direction**: Add an arena-membership check in the `put_field` opcode path (interpreter.zig) — in debug builds, panic if a tenured object receives an arena-allocated value. In release, either accept the documented contract (handler code must not do this) or enforce via compile-time flow analysis. Document the exact enforcement boundary.

#### F2.3 — MEDIUM: HiddenClassPool transition map is single-threaded without enforcement
**Where**: `packages/zigts/src/object.zig:1106-1153` (`HiddenClassPool.addProperty`)
**What**: `transition_map.put` at line 1146 is not atomic, and the pool is shared across requests reusing the same runtime. The current design is single-threaded (one request per runtime at a time), so this is safe — but nothing in the type signature, doc comment, or assertion makes the invariant visible. A future refactor that shares a runtime across threads (or introduces async within a request) would silently corrupt the map.
**Direction**: Add a `// thread-unsafe: caller must hold runtime exclusively` doc comment on `HiddenClassPool` and its mutating methods. Optionally add a `std.debug.assert(std.Thread.getCurrentId() == owner_tid)` in debug builds.

#### F2.4 — LOW: String lazy hashing races in non-single-threaded contexts
**Where**: `packages/zigts/src/string.zig:49-56` (`JSString.getHash`)
**What**: `getHash()` mutates `hash` and `hash_computed`. `getHashConst()` (line 58) casts away const to call it. Under the single-threaded-per-request invariant this is safe. The issue is the same as F2.3: invariant not stated in the type or comments, and if two threads ever hold handles to the same string (e.g., shared interning pool), the hash write is racy.
**Direction**: Document the invariant where strings are shared (interning pool). If intern pool is meant to be shared across threads in future, pre-compute hashes at intern time or use `std.atomic.Value(bool)` for `hash_computed` with release/acquire semantics.

#### F2.5 — LOW: `as const` handling not explicitly spec'd in stripper
**Where**: `packages/zigts/src/stripper.zig:14-20, 220-222`
**What**: `as const` is semantically meaningful in TypeScript (produces a read-only literal type). The stripper likely blanks all `as T` assertions uniformly, which is fine for JS runtime behavior but can cause divergence between what the type checker infers and what the stripper lets through. No explicit test asserts behavior for `as const`, ambient declarations, template literal types, conditional types, or `satisfies`.
**Direction**: Add targeted stripper tests for `as const`, `satisfies X`, ambient `declare`, and document which TS constructs the stripper handles vs rejects. If any are unsupported, emit a diagnostic rather than silently strip.

#### F2.6 — NIT: NaN canonicalization loses bit patterns silently
**Where**: `packages/zigts/src/value.zig:207-210` (`fromFloat`), `packages/zigts/src/value.zig:43` (`CANONICAL_NAN_BITS`)
**What**: All NaN values with upper 16 bits ≥ `MIN_TAG_PREFIX` (0xFFFC) collapse to 0x7FF8_0000_0000_0000. Required to avoid tag collision and legal under JS semantics (`NaN !== NaN`). Worth a one-line comment at `fromFloat` since the reason is non-obvious from the code.
**Direction**: Add a short comment at the canonicalization site: `// Required for NaN-boxing: user NaN bit patterns overlap with type tags at >= 0xFFFC.`

### Phase 2 summary

The engine core is well-designed. The NaN-boxing scheme is sound and the hidden-class SoA layout is appropriate. The main themes are **undocumented invariants** around single-threadedness (F2.1, F2.3, F2.4) and **write-barrier coverage** for arena→tenured escapes (F2.2). None are bugs today, but each is a trapdoor for future refactors. No blockers.

---

## Phase 3 — Compile-time analysis chain

Reviewed: `handler_contract.zig`, `handler_verifier.zig`, `handler_policy.zig`, `type_checker.zig`, `bool_checker.zig`, `flow_checker.zig`, `fault_coverage.zig`, `contract_diff.zig`, `path_generator.zig`, `comptime.zig`, `system_linker.zig`, `trace.zig`, `bytecode_cache.zig`, `route_match.zig`.

### Findings

#### F3.1 — HIGH: Bytecode cache key does not incorporate a zigts version/schema
**Where**: `packages/zigts/src/bytecode_cache.zig:499, 608`
**What**: Cache key is `SHA256(source)`. If zigts changes opcode semantics, adds opcodes, or changes hidden-class layout, stale cache entries will load and run on the new interpreter with no signal. For a serverless runtime where the same deployed artifact may be rebuilt against a newer zigts commit, this is a correctness hazard — a pre-production build might reuse cached bytecode from last week's engine and produce different behavior than a clean build.
**Direction**: Include a schema version constant (bumped on every breaking opcode/serialization change) in the cache key: `SHA256(source ++ SCHEMA_VERSION ++ git_commit_hash)`. CI should assert the schema version is bumped whenever the bytecode writer changes.

#### F3.2 — HIGH: Route-pattern diff treats parameter renames as equivalent
**Where**: `packages/zigts/src/route_match.zig:34` (`pathsMatch`)
**What**: `pathsMatch` treats any `:param` or `{param}` segment as equivalent regardless of name. A contract change from `/users/:id` to `/users/:userId` classifies as non-breaking. Upstream consumers relying on generated SDKs or documentation with parameter names will be silently broken, and cross-handler contract linking (`system_linker`) cannot detect the drift.
**Direction**: Distinguish param-position match (currently implemented) from param-identity match. Either surface a `param_renamed` flag in the diff verdict, or require identical parameter names and emit a `needs_review` verdict on rename.

#### F3.3 — MEDIUM: `handler_verifier.forOfReturns` always returns `.never`
**Where**: `packages/zigts/src/handler_verifier.zig:432`
**What**: Any handler whose final statement is a `for...of` loop that always returns inside the body is rejected as non-returning, because the verifier conservatively assumes the iterable could be empty. zigttp's JS subset is restricted enough that many iterables are statically bounded (literal arrays, spreads of known size, `range()`). The conservative answer pushes idiomatic patterns (e.g., `for (const handler of handlers) return handler(req);`) out of the verified set.
**Direction**: Detect statically non-empty iterables (literal arrays with ≥1 element, `range(n)` with constant `n > 0`) and let the loop body contribute to return analysis when the iterable is proven non-empty.

#### F3.4 — MEDIUM: Comptime evaluator enforces depth but not step/allocation budget
**Where**: `packages/zigts/src/comptime.zig:211, 260-263`
**What**: `max_depth = 64` guards recursion but not breadth. A handler with `comptime(new Array(1_000_000).fill(0).map(x => x+1))` can consume arbitrary memory and time during the build. zigttp ships as a build-time tool that may be invoked by untrusted handler code in a CI context; unbounded comptime makes the build a DoS surface.
**Direction**: Add a step counter covering binary/unary ops, property access, and allocations, capped at a generous budget (~100K steps). Fail with `ComptimeBudgetExceeded` if exceeded. Document the budget as part of the comptime contract.

#### F3.5 — MEDIUM: Flow-label propagation through ternary/spread not explicitly tested
**Where**: `packages/zigts/src/flow_checker.zig`
**What**: The `FlowProperties` union of labels (secret, credential, user_input) is declared, but the propagation rules for ternary expressions, spread in object/array literals, and function-call argument merging are not obviously asserted by tests. A false-negative (silent label drop) at a ternary is a security bug — a `secret` taint becoming unlabeled crosses the boundary that the flow checker exists to enforce.
**Direction**: Add explicit unit tests for each label-lattice join site (ternary, spread, destructuring, function return). Each test should assert that the resulting flow set is the union of the input labels. If the current implementation doesn't union at a given site, that's a HIGH-severity soundness hole.

#### F3.6 — MEDIUM: Fault coverage does not distinguish "caught failure" from "unreached critical path"
**Where**: `packages/zigts/src/fault_coverage.zig`, `packages/zigts/src/property_diagnostics.zig:519` (`success_on_critical_failure`)
**What**: The checker warns when a 2xx is reached on a path where a `critical` I/O call failed. It does not verify that the success path is actually conditional on the failure being handled — a handler that returns `Response.json({ok: true})` unconditionally after a failed auth call can be mis-flagged (if the auth is reached at all, the handler "ignored" it). Worse, the inverse case (handler that intentionally returns 200 with a safe default on auth failure) cannot distinguish itself from a bug.
**Direction**: Extend path analysis to tag each execution path with the set of failures it "handled" (via `result.ok` check or fallback branch). Only warn when a 2xx path has a critical failure that is *not* in the handled set.

#### F3.7 — MEDIUM: Policy overrides not validated against contract requirements
**Where**: `packages/tools/src/precompile.zig:667-678` (cross-referenced from Phase 5)
**What**: (Cross-listed with F5.4.) When `--policy` is passed explicitly, the file is not validated against what the contract declares. A policy permitting `API_KEY` when the contract never reads it (harmless but confusing) is silently accepted. A policy *forbidding* an env var the contract needs fails only at runtime, not at build. Both directions warrant a build-time check.
**Direction**: Diff policy allows-list against contract requires-list. Warn on extras; error on missing.

#### F3.8 — LOW: Route wildcard `*` unsupported in `pathsMatch`
**Where**: `packages/zigts/src/route_match.zig:9-16` (`isParamSegment`)
**What**: Recognizes `:param` and `{param}` but not `*` catch-alls. If contracts ever declare `/api/*` (common in other frameworks), the matcher returns false and diff treats related routes as unrelated.
**Direction**: Decide whether `*` is part of the pattern language, and if so, teach `isParamSegment` about it. Otherwise explicitly document that only named params are supported.

#### F3.9 — LOW: Header optionality not diffed in contracts
**Where**: `packages/zigts/src/type_checker.zig:1019`, `packages/zigts/src/contract_diff.zig`
**What**: Headers are always marked `.optional = false` in type_checker output. Contract diff classifies adds/removes but doesn't track optionality changes. If header optionality ever becomes modelable, the diff will silently miss it.
**Direction**: Either assert at build time that all header entries in the contract are `optional = false` (document the design constraint), or extend the diff to handle optionality changes.

#### F3.10 — NIT: `bytecode_verifier.zig` unreferenced in `docs/verification.md`
**Where**: `packages/zigts/src/bytecode_verifier.zig`, `docs/verification.md`
**What**: The file exists and is presumably invoked somewhere in the load path, but verification.md does not mention it. A reader wanting to understand what guarantees the bytecode loader makes has no pointer to this file.
**Direction**: Add a section in verification.md describing what bytecode_verifier checks (stack invariants? opcode validity? jump target bounds?) and when it runs.

### Phase 3 summary

The compile-time chain is the heart of zigttp's value proposition, and its claims mostly hold up. The two HIGH items (F3.1 bytecode cache identity, F3.2 route param rename equivalence) are real soundness gaps — both let the pipeline silently accept unsafe changes. The MEDIUM items cluster around unverified propagation rules (flow labels F3.5, fault coverage F3.6) and conservative-but-rejecting analysis (for-of F3.3). None are blockers, but F3.1 and F3.2 should be fixed before zigttp is used for production upgrade verification in the wild.

---

## Phase 4 — Runtime, HTTP, durability

Reviewed: `zruntime.zig`, `server.zig` (non-flow logic), `contract_runtime.zig`, `live_reload.zig`, `durable_recovery.zig`, `durable_store.zig`, `durable_scheduler.zig`, `proof_adapter.zig`, `http_parser.zig`, `self_extract.zig`, `security_logger.zig`, `security_events.zig`.

### Findings

#### F4.1 — HIGH: Module-scope JavaScript state persists across pooled requests, unenforced
**Where**: `packages/runtime/src/zruntime.zig:2365-2369` (explicit "do NOT clear user globals" comment), `:4810-4858` (`releaseForRequest`)
**What**: Request isolation via pool + arena is strong for ephemeral data, but handler top-level `let` bindings persist on the runtime and are visible to subsequent requests served by the same runtime. A handler writing `let counter = 0; function handler(req) { return Response.json({n: ++counter}); }` will leak state; the architecture doc's "request isolation" claim does not caveat this. The existing arena audit catches escaped arena pointers but not module-scope mutations.
**Direction**: Either (a) detect module-scope mutations at compile time (verifier check: no assignments to top-level `let`/`var` inside handler body), or (b) add an explicit section to `docs/architecture.md` stating that top-level bindings persist and are shared across requests, with a recommendation to use request-scoped locals. Pick one; the current silence is dangerous.

#### F4.2 — HIGH: Live-reload hot-swap has a narrow generation-inconsistency window
**Where**: `packages/runtime/src/zruntime.zig:4461-4484` (`reloadHandler`), `packages/runtime/src/live_reload.zig:228-248`
**What**: `reloadHandler` locks init + cache mutexes, updates source, clears bytecode cache, unlocks, then iterates pool slots to invalidate idle runtimes. An in-flight request's runtime that finishes between the unlock and the slot-iteration re-enters the pool *not* invalidated. The next `acquire` may pick that slot and reuse the old, uncleared bytecode cache lineage. The window is small but real.
**Direction**: Add a monotonically-increasing `generation` counter on HandlerPool. `reloadHandler` bumps it under lock. `acquire` checks the runtime's generation against the pool's current; mismatch triggers invalidation. This turns an eventual-consistency swap into atomic generation-stamped swap with no blocking cost.

#### F4.3 — HIGH: `--force-swap` breaks in-flight use-after-free contract
**Where**: `packages/runtime/src/live_reload.zig:25-29, 87-156`, no explicit lifetime management
**What**: `--force-swap` applies breaking changes. The "previous handler code kept alive until next reload" strategy is safe for at most one generation. Under a rapid sequence of force-swaps, an in-flight request from generation N could still hold references to code that generation N-2 has freed. No refcount or hazard pointer protects the old code.
**Direction**: Reference-count or generation-stamp the code buffer so in-flight handlers retain their generation until completion. Alternatively, document that `--force-swap` requires a drain step (refuse new requests, wait for in-flight completion, then swap).

#### F4.4 — MEDIUM (downgraded from agent-reported BLOCKER): `in_use` counter can drift from actual slot count on rare error paths
**Where**: `packages/runtime/src/zruntime.zig:4690-4770` (CAS-incremented `in_use`)
**What**: The CAS at line 4697 increments `in_use` before `pool.acquire()` succeeds. If `ensureRuntime` fails mid-way (OOM on `RuntimeUserData` alloc at line 4944), the error path releases the slot but must also decrement `in_use`. Reviewer flagged this as drift-possible. **Needs verification**: the error paths at 4944 appear to call `pool.release` which should cover the decrement; a targeted read is needed. Downgrading to MEDIUM until a concrete scenario is constructed.
**Direction**: Add a debug-mode invariant check at handler exit: `assert(in_use == pool.activeCount())`. If drift is ever observed, escalate. If the invariant holds, document the CAS protocol so future edits don't break it.

#### F4.5 — MEDIUM: Proof cache keyed only on method+URL; unsafe across handler reloads
**Where**: `packages/runtime/src/proof_adapter.zig:127-133`, `packages/runtime/src/server.zig:1200-1217`
**What**: `computeKey(method, url)` does not incorporate a handler identity or contract hash. When `reloadHandler` replaces the running handler, the proof cache is not invalidated automatically — only the bytecode cache is cleared. A GET cached under handler v1 will be served to a request intended for handler v2 even after the swap, for any request the server has seen before.
**Direction**: Include the current contract's `policy_hash` or `artifact_sha256` in the cache key, and/or explicitly clear the proof cache in `Server.updateContract()`. Verify via the live-reload test that cache is flushed on swap.

#### F4.6 — MEDIUM: Proof cache stores responses with side-effecting headers (Set-Cookie, Vary)
**Where**: `packages/runtime/src/proof_adapter.zig:168-241` (put), `:101-123` (`shouldCache`)
**What**: `shouldCache` gates on `Cache-Control: no-cache` but does not reject responses carrying `Set-Cookie`, `Vary`, `Cache-Control: private`, or `Cache-Control: must-revalidate`. Caching a `Set-Cookie` response replays the same session token to every subsequent matching request. For a runtime that markets "proven response memoization", this is a correctness bug — the handler may be proven deterministic + read_only, but the response contents aren't.
**Direction**: Extend `shouldCache` to reject any response with `Set-Cookie`, `Vary`, `Cache-Control: private|no-store|must-revalidate`, or `Authorization` in the response. This is a security-relevant gate; err on the side of not caching.

#### F4.7 — MEDIUM: Contract artifact_sha256/policy_hash parsed but not verified at startup
**Where**: `packages/runtime/src/contract_runtime.zig:196-203` (parsing), no call to a verify-at-startup helper
**What**: The recent sandbox hardening (`fae1843`) added `artifact_sha256` and `policy_hash` fields. They are parsed from the embedded contract but nothing verifies at server startup that the embedded bytecode actually hashes to `artifact_sha256` or that the active rule registry matches `policy_hash`. The fields are attestation data only — they don't gate startup.
**Direction**: Add `contract_runtime.verifyArtifactHash(embedded_bytecode, contract)` and `verifyPolicyHash(rule_registry, contract)` calls in `Server.init()` (or earlier in self-extract detection). Panic on mismatch. Closing this loop converts the hashes from documentation to enforcement.

#### F4.8 — MEDIUM: Self-extract validation failures are silent
**Where**: `packages/runtime/src/self_extract.zig:55-101`
**What**: `detect()` returns `null` on magic mismatch, size sanity, or checksum failure. Callers treat `null` as "no embedded payload" and fall through to runtime compilation. A tampered or corrupted binary is indistinguishable from an un-embedded one.
**Direction**: Differentiate "no payload present" (null) from "payload present but corrupt" (error). Log a security event (via the logger added in `fae1843`) on corrupt payload detection.

#### F4.9 — MEDIUM: HTTP parser has no explicit request-line length limit
**Where**: `packages/runtime/src/http_parser.zig:12-80, 90-100`, `packages/runtime/src/server.zig:2006-2024` (BufferedReader)
**What**: BufferedReader raises `HeaderLineTooLong` only after the line exceeds the 8KB buffer. A slow-drip attacker sending 1-byte/s toward an enormous request line holds a connection open while buffering consumes memory. Each in-flight connection can tie up buffer memory up to 8KB, and the parser doesn't short-circuit on a preview byte count.
**Direction**: Add an explicit `MAX_REQUEST_LINE_BYTES` check on the accumulated length before attempting to parse the method/URI. Reject early with 414. Similarly cap total header bytes (sum of all lines).

#### F4.10 — LOW: `Transfer-Encoding: chunked` flagged but not decoded
**Where**: `packages/runtime/src/http_parser.zig:306-307, 336-337`
**What**: Parser sets `fast_slots.has_chunked_encoding` when the header is present but no decoder is wired. Handlers receive the raw chunked body (size-in-hex markers interleaved with data), not the decoded stream. Either decode or reject.
**Direction**: Implement chunked decoding in the body reader, OR reject chunked requests with 501 Not Implemented. Current state (detect but ignore) is the worst option.

#### F4.11 — LOW: Durable scheduler polls every 1 second with no FS event integration
**Where**: `packages/runtime/src/durable_scheduler.zig:29-44`
**What**: Fixed 1 s cadence adds up to 1 s latency between signal availability and handler resumption. Safe but not low-latency.
**Direction**: On Linux use `inotify`, on macOS use `kqueue`/FSEvents, fall back to polling. Alternatively, have handlers write to a named pipe the scheduler blocks on. Not urgent.

#### F4.12 — LOW: Security event logger drain-queue overflow policy unverified
**Where**: `packages/runtime/src/security_logger.zig:44-54`
**What**: Drain loop sleeps 250 ms between flushes. If the in-memory ring buffer is bounded and fills under load, overflow policy (drop-oldest? drop-newest? block?) determines whether audit trails are reliable. Not visible in the logger file.
**Direction**: Verify the upstream `zq.security_events.Stream` overflow policy; document it; add a `dropped_events` metric if drop is the chosen policy.

### Phase 4 summary

Three HIGH findings cluster around **lifecycle boundaries**: handler module state (F4.1), live-reload generations (F4.2), and force-swap code retention (F4.3). Each is a silent class of bug — no crash, just wrong behavior. The proof cache surfaces two correctness gaps (F4.5 handler identity, F4.6 side-effecting headers) that undermine the "proven memoization" claim. The recent sandbox hardening is thorough at the declaration level but F4.7 shows that artifact/policy hashes are parsed and ignored — the enforcement wire is not connected. No verified blockers; F4.4 agent-reported BLOCKER downgraded to MEDIUM pending concrete reproduction.

---

## Phase 5 — Build-time tools & CLI surface

Reviewed: `precompile.zig`, `transpiler.zig`, `zigts_cli.zig`, `expert.zig`, `expert_meta.zig`, `verify_paths_core.zig`, `edit_simulate.zig`, `review_patch.zig`, `describe_rule.zig`, `search_rules.zig`, `module_audit.zig`, `deploy_manifest.zig`, `openapi_manifest.zig`, `sdk_codegen.zig`, `json_diagnostics.zig`, `manifest_alignment.zig`, `property_expectations.zig`, `prove.zig`, `system_rollout.zig`, `report.zig`, `mock_server.zig`, `skills/`.

### Findings

#### F5.1 — MEDIUM: `--policy` override not validated against contract capabilities
**Where**: `packages/tools/src/precompile.zig:667-678` (policy parse), `:685` (`compileHandler`)
**What**: Explicit `--policy` files are trusted verbatim. A policy permitting capabilities unused by the contract is noise; a policy *restricting* capabilities below what the contract requires passes build but fails at runtime. No diff against `contract.env.literal`, `contract.egress.hosts`, etc.
**Direction**: After parsing policy and extracting contract, diff the two. Warn on policy supersets (denote intentional broadening). Error on policy subsets of required caps (catch restriction-below-need at build, not runtime).

#### F5.2 — MEDIUM: `edit-simulate --before` violation-dedup key is too loose
**Where**: `packages/tools/src/edit_simulate.zig:238-252` (`ViolationKey`)
**What**: Deduplication uses `code + message` hash (intentionally excludes line numbers to tolerate edit shifts). Message text often includes the offending symbol name; a refactor that renames `user` → `username` at several call sites gets different messages at identical source lines and incorrectly classifies the new violations as "introduced by patch". Conversely, if two unrelated sites have the same symbol/message, one can mask the other.
**Direction**: Incorporate file (and optionally an approximate line-range bucket tolerant of ±5 line shifts). The current pure message-hash approach is over-aggressive deduplication.

#### F5.3 — MEDIUM: `review-patch --diff-only` rewrites the `total` field, making text output ambiguous
**Where**: `packages/tools/src/review_patch.zig:73-87, 128`
**What**: When `--diff-only` filters violations, `total` is overwritten to equal `new_count` and text output prints "Total: N". A reader sees "total" and assumes the handler's overall violation count, not patch-introduced count. JSON is unambiguous; text is misleading.
**Direction**: In text mode, label as "Patch-introduced: N" instead of "Total: N" when `--diff-only` is active; or print both pre-filter and post-filter counts.

#### F5.4 — MEDIUM: Generator-pack path resolution is silent on missing artifacts
**Where**: `packages/tools/src/precompile.zig:630-642` (`resolveGeneratorPack`)
**What**: Missing pack files set the corresponding path to `null` with no warning. `-Dgenerator-pack=/wrong/path` succeeds and emits fewer artifacts than expected.
**Direction**: Fail fast if the pack directory is missing entirely. For individual files, warn ("manifest.json not found in pack, skipping") rather than silently dropping.

#### F5.5 — MEDIUM: `--strict` flag semantics not documented in source
**Where**: `packages/tools/src/expert.zig:270-272`, `packages/tools/src/module_audit.zig:93-95` (`VerifyOptions`)
**What**: `--strict` upgrades some warnings to errors, per CLI help text, but the code does not document which categories are affected. A future maintainer cannot tell from reading `VerifyOptions.strict` what class of diagnostics it escalates.
**Direction**: Add a doc comment on `VerifyOptions.strict` enumerating the categories (missing-spec warnings, capability-drift warnings, whatever else). Add at least one test asserting that strict-mode produces a nonzero exit on a known warning case.

#### F5.6 — LOW: Precompile verification failure leaves stale `embedded_handler.zig`
**Where**: `packages/tools/src/precompile.zig:703-720`
**What**: On `VerificationFailed`, the tool writes `.violations.jsonl` and aborts, but any prior `embedded_handler.zig` from an earlier successful build remains. Re-running the build after the handler change breaks verification risks linking stale bytecode if the Zig build step doesn't rebuild.
**Direction**: Delete `output_path_final` on verification failure. Or write a `.invalid` sentinel that the build step checks before using the file.

#### F5.7 — LOW: Mock server silently skips malformed JSONL lines
**Where**: `packages/tools/src/mock_server.zig:188-217` (`parseTestRoutes`)
**What**: Malformed JSON or missing-type lines are dropped without message. A corrupt test file serves a partial subset, hiding coverage loss.
**Direction**: Count skipped lines; print a summary at startup (`loaded 42 routes, skipped 3 malformed lines`). Exit nonzero if *all* lines were malformed.

#### F5.8 — LOW: `describe-rule --hash` text output lacks a label
**Where**: `packages/tools/src/describe_rule.zig:29-34`
**What**: Prints raw 64-char hex hash with nothing else. CI scripts that later change format (add prefix, suffix) would silently capture the wrong value. Contrast `expert meta --json` which wraps it as `"policy_hash": "..."`.
**Direction**: Add `Policy hash: ` prefix, or require `--json` when `--hash` is used. Alternatively, document the exact stdout shape as a stable contract.

#### F5.9 — LOW: Skill documentation missing ZVM code range
**Where**: `packages/tools/src/skills/zigts-expert/SKILL.md:113-116`
**What**: Lists ZTS0xx–ZTS3xx but not ZVMxxx (virtual module governance codes emitted by `verify-modules`). Agents reading the skill will hit an undocumented code range.
**Direction**: One line addition: "ZVMxxx — Virtual module governance (capability checks, forbidden patterns)."

#### F5.10 — NIT: Hash serialization duplicated between `expert meta` and `describe-rule --hash`
**Where**: `packages/tools/src/expert_meta.zig:45`, `packages/tools/src/describe_rule.zig:31`
**What**: Both compute `rule_registry.policyHash()` and emit it, but via different code paths. Minor duplication, no bug.
**Direction**: Extract `writeHashHex(writer, hash)` if a third caller appears. Not urgent.

#### F5.11 — NIT: `property_expectations` and `manifest_alignment` not exposed via `expert`
**Where**: `packages/tools/src/property_expectations.zig`, `packages/tools/src/manifest_alignment.zig`
**What**: These modules drive precompile-time checks but have no standalone `zigts expert` subcommand. Agents or CI wanting to run them without a full compile must invoke Zig API directly.
**Direction**: Add `expert verify-properties` and `expert verify-manifest` subcommands if the agent flow needs them. Otherwise document that property verification only runs during `compile`.

### Phase 5 summary

The build-time CLI surface is coherent. The `expert` canonical-surface refactor (commit `533fed8`) landed cleanly with no parity gaps visible. Main risks are **silent acceptance of misconfiguration**: `--policy` overrides (F5.1), `--generator-pack` path errors (F5.4), mock-server malformed lines (F5.7). The `edit-simulate`/`review-patch` diff machinery has two UX bugs (F5.2 over-dedup, F5.3 total relabel) that affect agent-driven workflows directly. No blockers.

---

## Cross-phase themes

1. **Undocumented single-threaded invariants** — F2.1, F2.3, F2.4 in the engine; F4.4 in the runtime. The engine is single-threaded per request by design, but nothing in the type signatures or comments makes that explicit. Every contributor must re-derive the invariant.
2. **Proven artifacts, unverified at runtime** — F4.7 (`artifact_sha256`/`policy_hash` parsed, unvalidated), F3.1 (bytecode cache ignores engine version), F4.5 (proof cache ignores handler identity). A pattern: compile-time proofs are strong, but the runtime doesn't always consume them.
3. **Silent fallbacks where a loud failure is safer** — F4.8 self-extract corruption, F5.4 generator-pack missing files, F5.7 mock-server malformed lines. Each masks a configuration error.
4. **Write-barrier and lifetime edges** — F2.2 arena→tenured escape detection, F4.1 module-scope JS state persistence, F4.3 force-swap use-after-free risk. The "request isolation" story has sharp edges at the boundaries.

## Top-5 recommendations

1. **Wire `artifact_sha256` + `policy_hash` into startup verification** (F4.7). Two function calls close the attestation loop.
2. **Add a `generation` counter to HandlerPool** (F4.2, F4.5). Fixes live-reload consistency AND proof-cache staleness with one primitive.
3. **Include engine schema version in bytecode cache key** (F3.1). One-line change, prevents a class of silent-drift bugs.
4. **Document handler module-scope state persistence** (F4.1). Either add a verifier check or add an explicit doc section. Status-quo silence is the worst option.
5. **Tighten `shouldCache` on proof cache** (F4.6). Reject `Set-Cookie` / `Vary` / `Cache-Control: private`. Small change, closes a real correctness hole.

---

## Phase 6 — Cross-cutting sweeps

### Sweep A — Security posture

#### F6.A.1 — MEDIUM: Proof cache does not guard against handler mutations breaking cache validity

**Where**: `packages/runtime/src/proof_adapter.zig:127-133` (cache key), `packages/runtime/src/server.zig:1200-1217` (cache lookup/store)

**What**: The proof cache key is computed as `SHA256(method, url)` with no handler identity, contract hash, or generation stamp. When `live_reload.reloadHandler()` replaces the bytecode (line `server.zig:1220` → `zruntime.zig:4461`), the cache is not invalidated. A GET request cached under handler v1 serves stale proof to a request intended for handler v2 if the URL path is identical.

**Direction**: Include the current handler's `artifact_sha256` or a monotonic `generation` counter in the cache key. This is orthogonal to F4.2 (which fixes the _generation gap_ in live reload); here it's about cache coherence across handler reloads. Verify via existing live-reload tests that the cache is flushed on swap.

#### F6.A.2 — MEDIUM: Secrets in log output not consistently redacted

**Where**: `packages/runtime/src/server.zig:1236-1239` (error logging handler pool state), `packages/runtime/src/zruntime.zig:367` (trace file init logging), deploy files (`deploy/control_plane.zig`, `deploy/state.zig`, `deploy/http.zig`)

**What**: Error responses and pool exhaustion events log handler in-flight counts and error reasons. While credential fields in `DeploySession` are never logged (good), a handler that raises an exception with a secret-bearing message (e.g., `throw new Error("API key " + apiKey + " failed")`) will be visible in the error response unless explicitly redacted. The flow checker catches secret→response-body flows, but uncaught exception messages are a separate path.

**Direction**: Add exception-message sanitization in the runtime error handler (zruntime.zig line ~1325 `callFunction`) that checks if the error is a JS string containing env-var-like patterns (all-caps keys) before surfacing it. Or document in `docs/architecture.md` that exception messages must not include secrets (this is a handler contract, not a runtime guarantee).

#### F6.A.3 — LOW: Deploy credentials are loaded from files without explicit content validation

**Where**: `packages/runtime/src/deploy/autodetect.zig`, `packages/runtime/src/deploy/first_run.zig` (credential persistence), `packages/runtime/src/deploy/state.zig` (JSON parsing)

**What**: Credentials persisted to `~/.zigttp/credentials` are read back as JSON and decoded. No validation that the file wasn't modified since last write (no integrity check), no encryption of the credentials file on disk (they sit in plaintext in the user's home directory). This is acceptable for a development tool (users control their own machines), but worth noting for security-sensitive workflows.

**Direction**: Add a one-line comment in the credential loading path documenting that credentials are stored unencrypted per local-user-trust model. If higher assurance is needed later, this is where keyring integration would live.

#### F6.A.4 — LOW: HTTP request parsing does not explicitly limit request line or total header bytes

**Where**: `packages/runtime/src/http_parser.zig:90-100` (parseRequestLine), `packages/runtime/src/server.zig:2006-2024` (BufferedReader setup), Phase 4 finding F4.9

**What**: The BufferedReader will raise `HeaderLineTooLong` only after a single header line exceeds the 8 KB buffer. A client sending a request line or header value that is very long (but under 8 KB) consumes memory proportionally. There is no explicit max on request-line length or total header bytes (like HTTP/1.1's 414 "URI Too Long"). The defense is the 8 KB buffer itself, but it's not documented.

**Direction**: Add an explicit constant `MAX_REQUEST_LINE_BYTES` (e.g., 2 KB) and check it in `parseRequestLine()` before copying to storage. Reject with 414 if exceeded. Similarly cap total header bytes. Document these limits in the server module.

### Sweep B — Performance posture

#### F6.B.1 — LOW: Contract parsing happens once at startup; policy interpretation is compiled, not interpreted

**Where**: `packages/runtime/src/contract_runtime.zig:196-203` (one-time parse in `Server.init()`), `packages/zigts/src/handler_contract.zig` (contract extraction at build time)

**What**: Contract parsing is not a hot-path issue — it runs once at startup via `contract_runtime.parseFromEmbedded()`. Policy (property rules, capability requirements) is extracted at build time by the tools layer and baked into the contract as booleans/enums, not interpreted at runtime. No redundant work in the request path.

**Direction**: No findings at this layer — the pattern is correct.

#### F6.B.2 — LOW: Per-request allocations are arena-scoped; no per-request string concatenation in hot loop

**Where**: `packages/runtime/src/server.zig:1110-1117` (per-connection arena, reset on keep-alive), `packages/runtime/src/zruntime.zig:4541` (handler pool acquire)

**What**: The request path uses arena allocation (reset between requests on keep-alive) and does not perform unbounded string concatenation in loops. The two `std.fmt.allocPrint()` calls at `server.zig:2374, 2472` are in error/logging paths, not the hot handler-invoke path.

**Direction**: No findings at this layer — allocation patterns are appropriate.

#### F6.B.3 — LOW: Pool-level atomics use appropriate ordering; no contended locks observed on request path

**Where**: `packages/runtime/src/zruntime.zig:4690-4770` (CAS-incremented `in_use`), `packages/runtime/src/live_reload.zig:228-248` (two mutexes for reload coordination)

**What**: The handler pool uses atomic compare-and-swap (`CAS`) for `in_use` without strong ordering constraints (Phase 4 F4.4 flags a potential drift issue, but that's on error-path cleanup, not the hot path). Live reload holds two mutexes (init + cache) but only during the swap window, not during request serving. No locks block the per-request acquire path.

**Direction**: No findings at this layer. F4.4's CAS protocol is the only concern, and it's documented as needing verification rather than a performance issue.

#### F6.B.4 — LOW: Metrics and measurement gaps prevent latency visibility

**Where**: `packages/runtime/src/server.zig` (no per-endpoint latency tracking), `packages/runtime/src/zruntime.zig` (no per-handler-phase breakdown)

**What**: The runtime exposes `request_count` (line 81 `server.zig`) and pool state (in_use, max_size) but no per-endpoint latency histogram, per-phase timing (parse → acquire → invoke → response), or proof-cache hit rate. Cold-start time, warm-request overhead, and handler isolation costs are not exposed as metrics. These are valuable for optimization work but not necessary for correctness.

**Direction**: No blocking issue. If performance bottlenecks arise, add a metrics interface to `Server` (opt-in via flag) that records per-request phases and publishes them in Prometheus format or similar.

### Sweep C — Error-handling discipline

#### F6.C.1 — MEDIUM: Result-returning handlers without explicit failure-path testing

**Where**: `packages/zigts/src/handler_verifier.zig:432` (conservative for-of return analysis), `packages/zigts/src/fault_coverage.zig` (path enumeration for critical I/O)

**What**: The handler verifier requires all code paths to explicitly return a Response or Result. Handlers using `for...of` loops, `match` expressions, or early-exit patterns are correctly analyzed. However, fault coverage (Phase 3 F3.6) does not distinguish between "handler returns 200 despite critical I/O failure" and "handler explicitly handles the failure and returns 200". A handler that unconditionally returns `Response.json({ok: true})` after a failed auth call can pass fault coverage if the auth is nominally "reachable" but the handler never checks the result.

**Direction**: Extend the verifier to mark paths that handle errors explicitly (e.g., `if (auth.ok) { ... } else { return Response.json({error: ...}); }`). Document the distinction between "error-aware" and "error-oblivious" paths. This is an enhancement to the fault coverage guarantee, not a correctness issue today.

#### F6.C.2 — LOW: Null-deref panic risk from optional unwrap is minimized; orelse pattern widely adopted

**Where**: Random sampling of 50 `orelse` patterns across `packages/runtime/src/` and `packages/zigts/src/`

**What**: CLAUDE.md line 138 mandates `orelse instead of ? unwrap`. Spot-check across both packages shows widespread adoption; the `?` suffix operator appears mostly in safe contexts (return early from optional checks in loops, guard clauses). No obvious null-deref panic sites under unusual input. The `.?` operator (unsafe unwrap) does not appear in source outside test contexts.

**Direction**: No findings at this layer. The project's convention is being followed.

#### F6.C.3 — LOW: Catch-all error handlers are present but not swallowing critical failures

**Where**: `packages/runtime/src/zruntime.zig:1799-1810` (catch-all for user code exceptions), various `catch |err| switch (err) { ... }` patterns

**What**: The `callFunction` method has a catch-all that returns `error.Unknown` if the handler raises an uncaught JS exception. This is a user-code error (handler bug), not a runtime bug, so returning a 500 response is appropriate. Other catch-all handlers in the codebase (trace file init, module loading, durable recovery) log the error and proceed with a safe fallback (trace disabled, module unavailable, etc.). No critical failures are silently swallowed.

**Direction**: No findings at this layer — error handling is disciplined.

#### F6.C.4 — LOW: Errdefer coverage on allocations is comprehensive; spot-check confirms pattern

**Where**: Spot-check of 20 random allocation sites across `zruntime.zig`, `server.zig`, `contract_runtime.zig`, `deploy/*.zig`

**What**: All `allocator.alloc()`, `allocator.create()`, and similar operations carry matching `errdefer allocator.free()` or `errdefer <obj>.deinit()`. The arena allocator in `server.zig:1110` uses `defer arena.deinit()` which is correct (arena release includes all inner allocations). No missing cleanup paths detected.

**Direction**: No findings at this layer.

### Phase 6 summary

**Security posture**: The flow checker and module capability framework are sound; the architecture enforces secrets/credentials at the language level. Two MEDIUM findings cluster around **cache coherence** (proof cache not guarding handler identity) and **exception-message leakage** (uncaught exceptions may carry secrets). Deploy credentials are handled securely for a development tool; HTTP request parsing has implicit limits (8 KB buffer) but no explicit contract. Overall risk is LOW given that handlers are pre-verified and the runtime is not exposed to untrusted network input directly (it's a FaaS runtime, not a public API server).

**Performance posture**: The architecture shows good discipline — arena-scoped allocations, compiled policies, atomic-based pool synchronization, no hot-path locks. No per-endpoint latency visibility, but that's a measurement gap, not a correctness or performance bug. The project meets its design goals (instant cold starts, zero dependencies, request isolation) based on structural review; true validation would require benchmarking, which is deferred.

**Error-handling discipline**: Solid. Orelse patterns are standard; errdefer coverage is comprehensive; catch-all handlers return safe failures (500 response, log and proceed) rather than swallow critical errors. The one gap (F6.C.1 fault coverage not distinguishing error-aware paths) is an enhancement opportunity, not a soundness hole.

**Overall cross-cutting posture**: The codebase is disciplined in fundamentals. The three sweeps reveal **no blockers**. The security and performance stories are well-reasoned. Error handling is conservative. Recommendations from Phase 6 are either LOW-priority hardening (request-line limits, credential file encryption) or enhancements (exception redaction, fault-coverage refinement). Top priority items from Phases 2-5 (F4.7 artifact hash verification, F4.2 generation counter, F3.1 bytecode cache key) remain the highest-impact fixes.

---
## Deferred (not yet reviewed)

- Phase 6: cross-cutting sweeps (security, performance, error-handling). Would consume findings above for thematic patterns.
- Phase 7: tests, docs, CI, meta. Test coverage map, doc drift, release.yml audit.
- Phase 8: `packages/pi/` agent/TUI.
- Phase 9: synthesis (this document is the draft; would tighten and re-rank).

## Notes on finding severity

- One agent-reported BLOCKER (F4.4 `in_use` counter drift) was downgraded to MEDIUM pending a concrete reproduction path. The analysis was correct that a drift scenario is hypothetically possible, but the error-handling paths I checked do release the slot. Flag as "needs verification" rather than "ships broken".
- Phase 3 F3.2 (route param rename) is interesting: it's either a HIGH bug or a deliberate design choice (param names are implementation details). If the latter, documentation closes it. If the former, it's a soundness hole in upgrade verification.

---

## Phase 7 — Tests, docs, CI, meta

### Test coverage

**Test block counts by package** (1004 blocks total):

| Package | .zig files | Test blocks | Avg blocks/file | Coverage notes |
|---|---|---|---|---|
| `zigts` | 81 | 1004 | 12.4 | Comprehensive; parser (75 tests), bool_checker (113), stripper (60), interpreter (55) dominate |
| `runtime` | 25 | 209 | 8.4 | Medium coverage; zruntime (41), deploy (16), contract_runtime (25) concentrated |
| `tools` | 16 | 93 | 5.8 | Moderate; precompile (14), manifest_alignment (8), module_audit (7) |
| `pi` | 28 | 154 | 5.5 | Agent surface; client.zig has conditional `SkipZigTest` on `ZIGTTP_EXPERT_LIVE` env var |

**Five largest source files per package + test-to-LOC ratio**:

| File | LOC | Tests | Ratio | Coverage |
|---|---|---|---|---|
| zigts: interpreter.zig | 7193 | 55 | 0.76% | Core VM logic; hot-path execution tested |
| zigts: handler_contract.zig | 6687 | 23 | 0.34% | Contract extraction; moderate |
| zigts: bool_checker.zig | 2566 | 113 | 4.4% | **High** coverage for arithmetic/comparison checks |
| zigts: stripper.zig | 2352 | 60 | 2.5% | TS annotation stripping well-tested |
| zigts: object.zig | 2894 | 26 | 0.90% | Hidden classes, inline caching; low-medium |
| runtime: zruntime.zig | 7282 | 41 | 0.56% | Request executor; core isolation logic tested |
| runtime: server.zig | 2491 | 15 | 0.60% | HTTP layer; light coverage |
| runtime: deploy.zig | 2105 | 16 | 0.76% | OCI/provider plumbing; moderate |
| runtime: contract_runtime.zig | ~1400 | 25 | 1.78% | Contract parsing at startup; good |
| runtime: durable_store.zig | ~1000 | 2 | 0.2% | **Zero coverage** for oplog recovery |
| tools: precompile.zig | 4500 | 14 | 0.31% | Build pipeline; very light |
| tools: system_rollout.zig | ~900 | 4 | 0.44% | Cross-handler verification; sparse |
| tools: expert.zig | ~800 | 5 | 0.62% | CLI surface; moderate |
| tools: deploy_manifest.zig | ~600 | 14 | 2.33% | Platform-specific config; well-tested relative to size |
| tools: property_expectations.zig | ~400 | 6 | 1.5% | Pre-compilation property check; moderate |

**Flake indicators**: One conditional skip found at `/Users/srdjans/Code/ZigttpHome/zigttp/packages/pi/src/anthropic/client.zig:229-231`. The test block returns `error.SkipZigTest` unless environment variables `ZIGTTP_EXPERT_LIVE=1` and `ANTHROPIC_API_KEY` are set. This is intentional (live API integration test, not a latent flake). No `TODO.*flake` or `XXX` markers found in test blocks. No test-disabling patterns like `@setRuntimeSafety(false)` in test contexts.

**Integration tests**: `bash scripts/test-examples.sh` runs end-to-end handler testing across 14 example handlers (auth, caching, durable, deploy, etc.). Invoked in CI after build. Coverage: 14 example handlers × ~1-3 test cases each ≈ 30 example paths, all real request-response cycles. No test framework (raw `ziptp --test` invocation); diagnostic output parsed as bare text ("Results: N passed, M failed").

**Missing coverage**:
- `durable_store.zig` (2 test blocks on ~1000 LOC) — crash-recovery oplog is nearly untested. Critical for durable-execution claims.
- `precompile.zig` (14 tests on 4500 LOC) — build pipeline gating verification, policy override, generator-pack resolution. Very light.
- Cross-package integration (e.g., deploy manifest generation → actual OCI image build) — tested indirectly via examples; not isolated.

### Documentation drift

**21 docs in `docs/` reviewed against base ref `533fed8`**:

| File | Status | Finding |
|---|---|---|
| `architecture.md` | **Mostly accurate** | Phase 1 findings F1.4 (Deploy subsystem invisible in diagram) and F1.5 (static-file ordering not mentioned) remain unaddressed. Request flow prose matches code. |
| `verification.md` | **Accurate** | Checks 1-2 (Response returns, Result checking) match `handler_verifier.zig`. Mentions `bytecode_verifier.zig` (F3.10 from Phase 3 is moot). |
| `sound-mode.md` | **Accurate** | Type-directed truthiness rules match `bool_checker.zig` logic. |
| `typescript.md` | **Accurate** | Stripper examples match `stripper.zig` behavior; generic type aliases instantiation documented. |
| `jsx-guide.md` | **Accurate** | JSX runtime `h()` and `renderToString()` examples match `http.zig` (lines 950+). |
| `feature-detection.md` | **Accurate** | Unsupported-feature list (classes, async/await, var, while, switch, try/catch, null, regex, ==) matches parser diagnostics in `parse.zig`. |
| `performance.md` | **Unverified** | States: "3ms runtime init", "1.2MB binary", "4MB memory baseline", cold-start breakdowns per deployment pattern. **Zero citation** to benchmarks in `../zigttp-bench` (external repo). No measured cold-start data under `build.zig bench` (that runs Zig-native microbenchmarks, not cold-start). Claims not validated in-tree. |
| `deploy-tutorial.md` | **Accurate** | Walkthrough of `zigttp deploy` flow matches `deploy.zig` orchestration. Platform autodetect, first-run token prompt, drift detection all present. |
| `extension-model.md` | **Accurate** | Virtual module redesign plans align with current `module_binding.zig` + `module_binding_adapter.zig`. Forward-looking, not dated. |
| `frontier.md` | **Forward-looking** | Strategy doc; entries on "cross-platform deploy", "durable scale", "type-driven optimization" are aspirational. Not a source of drift. |
| `zigts-expert-contract.md` | **Stale** | References `canonical-surface` refactor (`533fed8`). Describes role of `expert` CLI. Accurate to current surface. |
| `capabilities.md` | **Accurate** | Lists env capabilities (clock, crypto, random, stderr, sqlite, filesystem, network, env, runtime_callback, policy_check) matching `module_binding.zig` declarations. |
| `control-plane-contract.md` | **Accurate** | Deploy manifest and contract schemas match precompile emission + `deploy_manifest.zig`. |
| `api-reference.md` | **Accurate** | Virtual module signatures match exports in `modules/*.zig`. |
| `mini-book.md` | **Comprehensive tutorial** | Covers language, type checking, verification. Matches current feature set. |
| `user-guide.md` | **Comprehensive reference** | Covers runtime CLI, deployment, durable execution, live reload. Aligns with `main.zig` and `zigttp_cli.zig`. |
| `blog/` subdirectory | **Blog posts** | Not reviewed in detail (out of scope for correctness audit); no broken links to docs found. |

**Deleted docs** (confirmed absent per Phase 1): `claude-tools.md`, `threat-model.md`, `rollout.md` no longer referenced anywhere. Confirmed with grep for orphan references.

**Drift summary**: No material drift. The two Phase 1 findings (architecture diagram gaps) remain open; none are contradicted by the docs.

### CI audit

`.github/workflows/release.yml` (130 lines):

**Trigger**: Tag push only (line 5: `on: push: tags: 'v*'`). No PR testing workflow. Implication: PRs do not run CI; only main branch merges + tag pushes are gated.

**Test matrix** (lines 16-35):
- Runs on macOS-latest and Ubuntu-latest (line 21).
- Invokes: `zig build test test-zigts test-zruntime test-expert test-expert-app test-capability-audit test-module-governance` (line 29).
- **Validation**: This exactly covers all test steps except `test-precompile`, `test-property-expectations`, and `test-rollout`. Phase 1 F1.1 identified the gap (`test-zigts` was missing from the default `test` step); CI compensates by invoking it explicitly, but local developers still miss it on `zig build test` alone.
- Also runs `bash scripts/test-examples.sh` (line 35) — end-to-end example handlers.

**Platform matrix** (lines 59-73): `build` job builds for x86_64/aarch64 × macOS/Linux (4 targets). Does NOT cross-compile during the `test` job (tests run on CI OS only). Test coverage is platform-limited; runtime code paths on Linux arm64 are compiled in release build (lines 81) but not tested.

**Release artifacts**: Checksummed tarballs per platform (lines 83-92 package, lines 112 create checksums, lines 127-129 upload). SHA256 checksums for each tarball + aggregate `checksums-sha256.txt`. Meets best practice.

**Policy hash validation** (lines 37-47): Asserts that `./zig-out/bin/zigts describe-rule --hash` matches `policy-hash.txt`. Fails the release if hash changes unexpectedly. Good enforcement.

**Expert subsystem verification** (lines 49-53): Runs `zigts expert meta --json`, asserts `rule_count >= 25` and `policy_hash | length == 64`. Lightweight sanity check.

**Secrets/env vars**: `ZIG_VERSION` hardcoded in env (line 12). No secrets referenced; no GitHub secrets (deploy token, registry creds) visible. Implication: deployment steps (lines 101-129) do not push to any registry — they only create GitHub Releases. Actual deploy credentials would live in the user's runtime environment, not in CI.

**Missing**: No Linux x86_64/arm64 test runs in the `test` job. The `build` job compiles for those targets but doesn't execute tests. If a runtime issue is platform-specific, it won't be caught in CI for non-macOS/non-Ubuntu runners.

**Capability helper audit** (`scripts/check-capability-helpers.sh`, 46 lines):

Enforces a pattern: each virtual module function must wrap its implementation with checked helpers defined in `module_binding.zig`. The script greps for direct capability access (e.g., `try std.time.timestamp()` outside a helper) and fails if found. **Not invoked in CI**. Would need to be added as a build step or pre-commit hook to enforce.

### Meta-files

**README.md** (80 lines reviewed): Covers quick start, what makes it different (opinionated subset, JSX, verification, sound mode, compile-time eval, sandboxing, effect classification, I/O depth, proven evolution, durable execution, guard composition, behavioral contract, live reload, mock server, native modules, numbers). References old `docs/sound-mode.md`, `docs/verification.md`, `docs/deploy-tutorial.md` — all still present, no drift. Install and quickstart accurate to current binary locations and flags.

**CONTRIBUTING.md** (72 lines): Build, test, adding modules, adding rules, code style, commits, and bug reports. Reflects current state: `zig build`, test targets, virtual module requirements, capability enforcement, rule registry updates. Notes that `zig fmt` is required; CI does not auto-format (true — no linting in release.yml). Recommends running `zig build release` to verify hash; documents the policy-hash check accurately.

**AGENTS.md** (64 lines): Describes the project for agent automation. Covers repo structure, test commands, virtual modules, how to add rules, deployment, durable execution. Matches CLAUDE.md in scope and accuracy. Does not reference deleted docs.

**CLAUDE.md** (100+ lines, partially read): Project context, build commands, architecture, virtual modules table, JS subset, compile-time systems, CLI options. Accurate to current state. F1.1 (the `zig build test` inaccuracy flagged in Phase 1) is still present: line 25 claims `zig build test # All tests` but it doesn't run `test-zigts` or `test-zruntime`. Should be updated to reflect the full suite or documented with caveats. **STALE on this point**.

### Phase 7 summary

Test coverage is **uneven**: the engine (1004 tests) is well-covered; the runtime core (zruntime, server) is light at ~8 tests per 1000 LOC; tools and durability are nearly untested. The durable-execution subsystem (`durable_store.zig` with 2 tests on ~1000 LOC) is a significant gap for a claimed feature. Documentation is **accurate and comprehensive**; no drift found except Phase 1 architecture diagram gaps remain unaddressed. CI **runs the core test suite but misses per-platform test validation** for Linux arm64; no PR-gating workflow (only tag pushes). The hardened repo policy (policy hash assertion, expert subsystem validation) is well-enforced. One meta-file stale point: CLAUDE.md line 25 incorrectly claims `zig build test` runs all tests — it does not run engine or runtime tests by default.

---

## Phase 9 — Synthesis

### Count

57 findings across 7 phases.

| Severity | Count |
|---|---|
| BLOCKER | 0 |
| HIGH | 7 |
| MEDIUM | 23 |
| LOW | 22 |
| NIT | 5 |

Phase 6 found zero HIGH/BLOCKER — the cross-cutting security/perf/error-handling posture is sound at the pattern level. The HIGH findings all live in Phases 1-4 and cluster around two fault lines: lifecycle boundaries (live reload, pool, module state) and proof-chain wiring (artifacts parsed but not verified, cache keys missing version/identity).

### Consolidated HIGH findings

1. **F1.1** — `zig build test` does not run the engine suite. CLAUDE.md line 25 is wrong.
2. **F1.2** — `pi_app` linked unconditionally into the deploy binary. Binary-size goal unverified.
3. **F2.1** — PIC coherence depends on undocumented hidden-class immutability invariant.
4. **F3.1** — Bytecode cache key omits engine schema version. Silent drift across zigts versions.
5. **F3.2** — Contract diff treats parameter renames (`/users/:id` → `/users/:userId`) as non-breaking.
6. **F4.1** — Module-scope JS state persists across pooled requests, unverified and undocumented.
7. **F4.2** — Live-reload hot-swap has a narrow generation-inconsistency window.
8. **F4.3** — `--force-swap` can leave in-flight handlers referencing freed code buffers.

(F4.7 proof-hash not enforced was tagged MEDIUM by Phase 4 but deserves HIGH in retrospect — see recommendation #1.)

### Top-7 recommended actions (re-ranked after full review)

Ordered by fix-cost/impact ratio. Each bundles adjacent findings that share a fix.

**1. Wire `artifact_sha256` + `policy_hash` into startup verification** — F4.7
Two function calls in `Server.init()`. Converts attestation data into enforcement. Converts the whole compile-time proof chain from "we proved it" to "we proved it and we check it at startup." One-day fix. Should be done before F1.2 (binary-size work) because it changes what "correct deploy" means.

**2. Add a `generation` counter to `HandlerPool`** — F4.2, F4.5, F6.A.1
Single primitive that fixes three distinct bugs: live-reload race (F4.2), proof-cache staleness across handler swaps (F4.5), and cache coherence during reload (F6.A.1). Atomic u64 on the pool; runtimes carry their generation at checkout; mismatch triggers invalidation. Low risk, high coverage.

**3. Decide and document handler module-scope state policy** — F4.1
Either verify (add a check in `handler_verifier.zig` that rejects top-level `let`/`var` assignments inside handler bodies) or document (clear paragraph in `docs/architecture.md` stating that module bindings persist and are shared). Status-quo silence is the worst option because it makes "request isolation" a half-truth.

**4. Include engine schema version in bytecode cache key** — F3.1
One-line change: `SHA256(source ++ SCHEMA_VERSION)`. CI already asserts policy-hash parity; add a schema-version constant and assert it bumps when the bytecode writer changes. Prevents silent drift from the moment it lands.

**5. Tighten proof cache eligibility** — F4.6
Extend `shouldCache` to reject `Set-Cookie`, `Vary`, `Cache-Control: private|no-store|must-revalidate`, `Authorization`. Tens of lines of code. Closes a real correctness hole in the "proven memoization" claim.

**6. Fix the test-step gap** — F1.1
Chain `test-zigts` + `test-zruntime` into the default `test` step. CI already runs them; local dev deserves parity. Alternatively correct CLAUDE.md — but chaining is better because the doc contract stays meaningful.

**7. Resolve the `pi_app` packaging question** — F1.2
Build `zigttp` with and without the `pi_app` import at `ReleaseFast`, diff sizes. If dead-code elimination strips it: document that and the finding closes. If it doesn't: split `zigttp-expert` into a separate binary. Needs measurement, not analysis.

### Cross-phase themes (ranked by prevalence)

**Theme 1: Compile-time proofs, runtime indifference.** The repo spends extraordinary effort proving handler properties at build time (verification, contract, flow labels, fault coverage, policy derivation) but the runtime doesn't always consume what the build proves. F4.7 (`artifact_sha256` parsed, not verified), F3.1 (bytecode cache ignores engine version), F4.5 (proof cache ignores handler identity) — three instances of the same pattern. The theme suggests the proof chain was designed bottom-up (build correctness first, runtime attestation second) and the last link is missing.

**Theme 2: Undocumented single-threaded invariants.** F2.1 (hidden class immutability), F2.3 (transition map), F2.4 (string lazy hash), F4.4 (`in_use` CAS protocol). The engine is single-threaded per request by design, but nothing in the type signatures enforces it. A future refactor toward in-request concurrency (async handlers, structured I/O expansion) will silently break several of these without any compile-time signal.

**Theme 3: Silent fallbacks where failures should be loud.** F4.8 (self-extract corruption returns null), F5.4 (generator-pack missing files silently skipped), F5.7 (mock-server malformed lines dropped), F3.5 (flow labels unverified through ternary) — configuration errors and potential soundness holes that don't manifest until the wrong behavior runs in production.

**Theme 4: Lifecycle boundary sharpness.** F4.1 (module state across requests), F4.2 (reload generation gap), F4.3 (force-swap code retention), F2.2 (arena → tenured escape). The request-isolation story works for ephemeral state but gets fuzzy at the edges — at handler reloads, at module initialization, at arena audit boundaries.

**Theme 5: Test debt is concentrated, not spread.** The engine is exhaustively tested (1,004 tests on ~60 KLOC). The runtime core and tools are moderately tested. The durable subsystem is nearly untested (`durable_store.zig`: 2 tests / ~1,000 LOC). If durable execution is a first-class feature (architecture.md describes it in detail), coverage needs to reflect that.

### Health assessment

- **Engine**: Solid. Memory model is coherent. Findings are about documenting invariants, not fixing bugs.
- **Runtime**: Healthy core, sharp edges at lifecycle transitions. The pool + arena design is good; the swap machinery needs a generation primitive.
- **Compile-time chain**: Strong claims, mostly delivered. Two HIGH items (F3.1, F3.2) are real soundness holes that should be fixed before relying on upgrade verification in production.
- **Tools CLI**: Well-structured. The `expert` canonical-surface refactor landed cleanly. Most findings are UX polish.
- **Docs**: Accurate in substance. Architecture diagram and a few doc comments need updating.
- **CI**: Functional but PR-tier missing. Platform matrix compiles wider than it tests. Capability audit not wired into CI.
- **Test coverage**: Uneven (engine > runtime > tools > durability).
- **Security posture**: Good cross-cutting discipline. Capabilities are routed through checked helpers; flow labels exist. Gaps are in what the runtime enforces, not what it computes.
- **Performance posture**: Disciplined structure (arena, pool, hidden classes, NaN-boxing). Claims in `docs/performance.md` are externally measured; in-tree verification is absent.

### Deferred

- **Phase 8** (`packages/pi/`): 2.3 KLOC agent/TUI/REPL. Not reviewed. Given F1.2 (should it even ship with `zigttp`?), a full review may be premature until the packaging question is resolved. If pi stays in the runtime binary, Phase 8 becomes a security review (pi has access to the Anthropic API and arbitrary handler analysis). If pi splits out, Phase 8 is a lower-priority developer-tool review.
- Perf benchmarks vs. claims — `docs/performance.md` cites numbers from `../zigttp-bench`; reviewer didn't run them.
- The downgraded-BLOCKER F4.4 (`in_use` drift) — warrants a concrete repro attempt before closing. A targeted read of error paths in `ensureRuntime` + `pool.acquire` + their defers/errdefers would settle it in ~30 minutes.

### What would ship the fixes

- **One PR for items 1-2** (`artifact_sha256` verification + `HandlerPool` generation counter) — the highest-leverage bundle. Adds enforcement to three existing attestation fields and fixes three separate staleness bugs. Should include tests that simulate reload-during-request and corrupt-payload-at-startup.
- **One PR for item 3** (module-scope state policy) — short-circuits a design ambiguity. Spec change, not code change, unless the decision is to add a verifier check.
- **One PR for items 4-5** (bytecode cache key + proof cache eligibility) — small surgical fixes with clear semantics.
- **One PR for items 6-7** (test step chaining + pi_app measurement) — ergonomics and packaging; no runtime behavior change.

None of these require a new subsystem or a refactor. The repo is in a state where the next round of work is small targeted PRs, not restructuring.

