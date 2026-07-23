# Advisory Plan Index

Originally planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`).
Re-validated and extended at commit: `560239d` (`feat(expert,cli): inline approval diff, ...`) on 2026-06-20.
Extended at commit: `a4a731bd` (`docs(changelog): note the recoverOne double-free fix`) on 2026-07-02.
Extended at commit: `b00eae29` (`fix(modules): stop fetchWithRetry sleeping past the request deadline`) on 2026-07-10.

This directory contains source-backed remediation plans from read-only advisor passes. The advisor did not modify production source. Each plan is self-contained for a fresh-context executor.

## Pass at `b00eae29` (2026-07-10)

A fresh, repository-wide review of the threaded runtime, ZigTS compiler/VM and proof passes, virtual-module boundary, and embedded Pi agent. Every finding below was re-opened against the live source; earlier advisory notes were used only as hypotheses. No production source changed in this pass.

### Verification snapshot

```sh
bash scripts/verify.sh
```

The full local CI mirror passed: aggregate/unit roots, runtime purity, docs and module governance, ReleaseFast build, v1 deploy smoke, panic isolation, all 41 example suites, policy-hash guard, semantics/Z3 gate, and expert-subsystem verification.

### Recommended execution order

| Order | Plan | Priority | Effort | Risk | Why here |
| --- | --- | --- | --- | --- | --- |
| 1 | [015-enforce-agent-tool-effect-boundaries.md](015-enforce-agent-tool-effect-boundaries.md) | P0 | M | MED | Restores the agent's documented authorization invariant: no model/RPC tool can write around approval. |
| 2 | [016-own-and-bound-websocket-workers.md](016-own-and-bound-websocket-workers.md) | P0 | M-L | MED-HIGH | Removes detached-worker use-after-free risk and caps remotely created kernel threads. |
| 3 | [018-fail-closed-on-analysis-allocation-errors.md](018-fail-closed-on-analysis-allocation-errors.md) | P0 | L | MED | Prevents partial proof state from becoming a clean compiler result under allocation failure. |
| 4 | [017-serialize-websocket-outbound-frames.md](017-serialize-websocket-outbound-frames.md) | P1 | M | MED | Makes concurrent game broadcasts protocol-correct and shares outbound close validation. |
| 5 | [019-align-websocket-module-contract.md](019-align-websocket-module-contract.md) | P1 | M | MED | Removes advertised-but-throwing API, fixes signature drift, and stops silently omitting room peers. |

Plans 015, 016, and 018 are independent. Plan 017 can execute before 016 only if it proves outbound-state lifetime independently. Plan 019 should follow 016 so complete room snapshots inherit a finite connection bound.

### Status (executed 2026-07-10)

| Plan | Status |
| --- | --- |
| 015 | DONE (mandatory strongest-effect metadata; model/RPC policy gates; single compiler-veto edit path; provider and RPC regressions) |
| 016 | DONE (joinable server-owned workers; pre-upgrade admission cap; race-safe fd handoff; graceful live-socket shutdown) |
| 017 | DONE (retained per-peer outbound writer; complete-frame serialization; unregister safety; close validation and concurrency regressions) |
| 018 | DONE (sticky allocation failure across all five proof passes and pipeline; separately approved follow-up splits `TypePool` operational failure from semantic `null_type_idx`) |
| 019 | DONE (removed `roomFromPath`; aligned three-argument auto-response contract; complete room snapshots; real two-peer chat gateway gate) |

### Verified findings not planned in this batch

- **Sensitive workspace reads are unrestricted inside the repo**: `workspace_read_file` enforces containment but no secret-path policy; `workspace_list_files` excludes build noise only; tool output persists by default. This is a real privacy/least-privilege gap, but the correct default-deny/approval/persistence policy needs a product decision before an executable plan.
- **OpenAI model selection and Pi documentation drift**: the active backend supports OpenAI, but the model registry contains only Claude ids; `/model` and `--model` can therefore send a Claude id to OpenAI, while the OpenAI default cannot be selected from the registry. `packages/pi/README.md` still calls OpenAI deferred. Small, high-confidence follow-up.
- **WebSocket runtime callbacks have almost no direct execution coverage**: pool/codec units exist, but send/close/module-callback behavior and the shipped chat example are not exercised end to end. Plan 019 absorbs the game-facing portion.
- **Crypto/SQL wrapper execution coverage remains thin**: compiler/governance and lower SDK helpers are tested, but `zttp:crypto` base64/HMAC wrapper behavior and `zttp:sql` parameter/row conversion are not run through their module callbacks against known vectors/a temporary database.
- **Contract JSON serialization in `build_command.zig` swallows writer failure**: a partial/empty contract can reach later artifact assembly instead of returning the allocation/write error. Small fail-closed cleanup; lower leverage than plan 018's proof-pass issue.
- **Provider-neutral model metadata is absent**: this is the root architectural cause of the OpenAI selection bug; model ids need a provider tag and backend-filtered listing/validation.

### Strategic findings already tracked elsewhere

- Continue the roadmap's strict engine/runtime facade and incremental `zruntime.zig`/`server.zig` concern extraction; do not bundle it with lifecycle fixes.
- Keep JIT loop/tier deduplication in `DEFERRED_VM_LOOP_DEDUPE_PLAN.md`; the current interpreter/baseline/optimized parity gate is the correct prerequisite.

## Pass at `a4a731bd` (2026-07-02)

A full architecture + low-level-implementation review of `packages/runtime/`, `packages/zts/`, `packages/tools/`, `packages/modules/` (four parallel focused audits: architecture, performance/hot-path, Zig idioms, maintainability), followed by direct re-verification of every finding below against the live source at `a4a731bd` before writing plans. This pass did not re-open or re-verify plans 001-009 (their statuses below are carried over unchanged from the `560239d` pass) and did not run `scripts/verify.sh` (read-only vetting only — no code was changed, so there is nothing new to gate; each new plan's own verification commands must be run by its executor).

Plans 010-014 are new this pass, each vetted by opening the cited files myself (not just trusting the sub-agent audit passes that first surfaced them).

## Reconciliation note (pass at `560239d`)

Plans 001-005 were planned at `f2c637d`. The only commit since (`560239d`) touched `packages/pi/src/{loop,repl}.zig` and `packages/runtime/src/{dev_cli,init_command}.zig` - none of which any of 001-005 cite - so **all five remain valid at `560239d`** (their drift checks against `f2c637d` are still empty for their in-scope files).

- **001 (format baseline)**: resolved. `zig fmt --check build.zig packages/` passes on the current tree. A clean checkout of `560239d` still fails the gate because that historical commit predates the formatting fix.

Plans 006-009 are new this pass, each vetted by opening the cited files.

## Verification Snapshot

Gates run this pass (all green except the committed-state format gate, which 001 addresses):

```sh
zig fmt --check build.zig packages/        # passes on the working tree (001 fix present); fails on a clean checkout of HEAD
zig build test-expert-app test-expert test-cli
zig build test-docs-drift test-doc-links test-expert-golden test-cassette
```

The prior pass (`f2c637d`) additionally ran the full suite set; see git history of this file.

## Recommended Execution Order

Ordered by leverage (prerequisites and high-confidence security float up).

| Order | Plan | Priority | Effort | Risk | Why here |
| --- | --- | --- | --- | --- | --- |
| 1 | [001-restore-format-baseline.md](001-restore-format-baseline.md) | P1 | S | LOW | Restores the CI/documented format gate. Fix already in working tree; just commit. |
| 2 | [007-add-verify-gate.md](007-add-verify-gate.md) | P1 | S | LOW | Prerequisite: a one-command local gate mirroring CI, so every later plan's verification is trustworthy. |
| 3 | [006-validate-iso-date-time-ranges.md](006-validate-iso-date-time-ranges.md) | P1 | S-M | LOW | Security fail-open on the `.validated` trust boundary; small, fix logic already exists in `time.zig`. |
| 4 | [002-fix-pool-acquire-timeout.md](002-fix-pool-acquire-timeout.md) | P1 | M | MED | `max_retries=100` circuit breaker can fail before the configured `acquire_timeout_ms`. |
| 5 | [003-populate-proof-facts-summary.md](003-populate-proof-facts-summary.md) | P1 | M | LOW | Proof HUD/ledger omit contract intent and behavior counts. |
| 6 | [008-fix-io-module-return-metadata.md](008-fix-io-module-return-metadata.md) | P2 | S | LOW | `zttp:io` `parallel`/`race` declare `.string` but return array/Response; misleads type checker, `.d.ts`, expert. |
| 7 | [009-reject-unknown-cli-flags.md](009-reject-unknown-cli-flags.md) | P2 | S | LOW-MED | `features`/`modules`/`meta`/`describe-rule`/`search` silently drop unknown flags. |
| 8 | [004-correct-sparse-array-callbacks.md](004-correct-sparse-array-callbacks.md) | P2 | M | MED | Sparse-array callbacks have observable JS semantics gaps. |
| 9 | [005-use-utf16-string-indexing.md](005-use-utf16-string-indexing.md) | P2 | M | MED | Non-ASCII string indexing is byte-based in scoped methods. |

Status values below: TODO | IN PROGRESS | DONE | BLOCKED (<reason>) | REJECTED (<reason>)

| Plan | Status |
| --- | --- |
| 001 | DONE (formatting-only fix committed; `zig fmt --check` + test-zruntime/test-server green) |
| 002 | DONE (removed the retry-count circuit breaker so a nonzero acquire_timeout_ms is the sole wait bound; timeout 0 still fails fast; retry_count saturates; +2 tests, one proven to fail on the old breaker; test-zruntime/test-server + zig build test green) |
| 003 | DONE (factsFromContract now calls setIntentSummary via intentSummaryFromContract helper, so HUD+ledger render intent assertion/dynamic + behavior/failure path counts; +1 regression test proven to fail without the wiring. NOTE: live_reload.zig tests run under test-cli, not test-zruntime as the plan's run line said; test-cli + test-proof-review + zig build test green) |
| 004 | REJECTED (hits its own STOP conditions: this JS subset has no sparse arrays. Array elisions `[1, , 3]` and `delete arr[i]` are hard parse errors, `new Array(n)` is unsupported, and the remaining `arr.length=N`/`arr[i]=x` paths store gap slots as `undefined_val` - bit-identical to explicit `undefined` (all backing inits to `undefined_val`; `getIndex` collapses both to null). The object model cannot distinguish hole vs explicit undefined without a hole-sentinel storage change larger than this plan. Worse, the prescribed "skip holes" change would REGRESS the common, fully-supported case `[1, undefined, 3].forEach(f)` by wrongly skipping index 1; since `undefined` is this subset's documented sole absent-value sentinel, visiting every in-length index (current behavior) is correct and consistent. No code change.) |
| 005 | DONE (stringLength/charAt/slice/substring now index by UTF-16 code units, not UTF-8 bytes: added utf16Length, utf16IndexToByteOffset (rounds astral splits down to a codepoint boundary), charCodepointSliceAt, stringIsAscii. ASCII keeps an O(1) fast path (length avoids rope flatten); zero-copy SliceString path retained on byte offsets. Astral charAt returns the whole codepoint (lone surrogate is unrepresentable in UTF-8) - documented limitation. +1 regression test proven to fail on byte-length (`"é".length` expected 1 found 2). test-zts (1327 pass) + full scripts/verify.sh green, fmt clean, policy hash unchanged. FOLLOW-UP (code-review max): the initial pass left indexOf/lastIndexOf returning BYTE offsets and the comptime evaluator byte-indexed, so `s.slice(s.indexOf(t))` and `comptime("é".length)` disagreed with the migrated methods for non-ASCII. Completed the migration: indexOf/lastIndexOf now take a code-unit position and return a code-unit index; comptime length/charAt/slice/indexOf use UTF-16; the UTF-16 primitives moved to string.zig (utf16Length/utf16IndexToByteOffset/utf16AdvanceToUnit/byteOffsetToUtf16Index/charCodepointSliceAt) as the shared source of truth; slice/substring now fuse start+end mapping into one continuous walk (3 scans -> 2 on the non-ASCII path). +indexOf/lastIndexOf regression (proven to fail returning byte 6 vs 5) and +comptime UTF-16 test. test-zts (1328 pass) + scripts/verify.sh green.) |
| 006 | DONE (validateIsoDate/Datetime now range-check month/day (leap-aware via pub time.daysInMonth), clock fields, and bound the trailing fractional+tz grammar; +9 regression cases; test-modules + zig build test green, fmt clean) |
| 007 | DONE (scripts/verify.sh mirrors ci.yml test job - 9 steps + format note; CLAUDE.md test claim corrected; full `bash scripts/verify.sh` green, exit 0) |
| 008 | DONE (zttp:io parallel/race return kind string->object in both io.zig binding and io.json spec; +registry assertion test in builtin_modules.zig; modules --json now reports object. Also regenerated meta.golden.json: module_registry_hash legitimately changed (it digests return kinds) - only that line. test-zts/governance/modules + examples (27/27) + zig build test green) |
| 009 | DONE (features/modules/meta/describe-rule/search now reject unknown flags with error.InvalidArgument, surfaced as the clean dev_cli "invalid arguments" message + exit 1; `--json`/`--help`/`-h`/positional still work. Added isHelpToken helper in zts_cli; removed orphaned hasFlag in expert.zig. NECESSARY DEVIATION from the in-scope file list: tests are 10 `addExpertExitCheck` E2E assertions in build.zig (reject exit 1 + accept exit 0, run against the real `zttp` binary in the test-expert-golden step, which is in `zig build test`), NOT unit tests - because zts_cli.zig/describe_rule.zig/search_rules.zig are only reached via the `zts_cli` named module and have NO test root, so their `test {}` blocks are collected by no suite (proven via an aggregate canary: `expect(false)` in zts_cli.zig left `zig build test` green). Rooting zts_cli.zig directly also fails to compile because describe_rule.zig has PRE-EXISTING dormant tests using the 0.16-removed `std.io.fixedBufferStream` (lines ~238/254) - left untouched as out-of-scope, see note below. The exit-check harness is the codebase's own pattern (build.zig:597-603). Red-proof: reverting modules' reject branch made `zttp modules --josn` exit 0, failing the exit-1 check. zig build test + test-cli + test-zts + full scripts/verify.sh green, fmt clean, policy hash unchanged.) |

## Recommended Execution Order (pass at `a4a731bd`, 2026-07-02)

Ordered by leverage (prerequisites and high-confidence availability/security issues float up). All five are independent of each other and of plans 001-009.

| Order | Plan | Priority | Effort | Risk | Why here |
| --- | --- | --- | --- | --- | --- |
| 1 | [010-fix-chunked-body-quadratic-reparse.md](010-fix-chunked-body-quadratic-reparse.md) | P1 | M | LOW | Remotely triggerable O(n^2) CPU amplification on any chunked request split across reads; well-tested surface, low fix risk. |
| 2 | [011-cap-reuse-unbounded-atom-growth.md](011-cap-reuse-unbounded-atom-growth.md) | P1 | M | LOW | The framework's own best-case pooling policy (`reuse_unbounded`, granted to pure/deterministic/state-isolated handlers) has no ceiling and eventually hard-fails `JSON.parse`/property access until process restart. |
| 3 | [012-guard-bytecode-cache-enum-decode.md](012-guard-bytecode-cache-enum-decode.md) | P1 | S/M | LOW | Malformed/corrupted/version-skewed embedded bytecode panics the whole server with no fallback, unlike the sibling dev-cache path which already recovers gracefully from the same failure class. |
| 4 | [013-dedupe-json-string-escaping.md](013-dedupe-json-string-escaping.md) | P1 | S | LOW | Two drifted local copies of JSON string escaping silently emit RFC-8259-invalid JSON on control-character input; small, mechanical, high-confidence fix. |
| 5 | [014-fix-module-compiler-panicking-parser-init.md](014-fix-module-compiler-panicking-parser-init.md) | P2 | S | LOW | Contradicts the documented live-reload guarantee ("compilation failures keep the currently serving handler active") on OOM; smallest, most contained fix of the five. |

## Status (pass at `a4a731bd`)

Executed 2026-07-02 via five parallel worktree-isolated agents (one per plan), each committed independently then merged into `main` in recommended order with manual conflict resolution and re-verification after each merge. Note: the worktree isolation mechanism branched each agent from `origin/main` (`87373f59`, about a day behind local `main` at the time) rather than local `main` - each agent's own drift check caught this, confirmed no meaningful mismatch against the plan's cited excerpts, and proceeded; merges back into local `main` were done as real 3-way merges (not fast-forwards) with the actual diffs inspected and full suites re-run against the merged tree before each merge commit, not just trusted from the isolated worktree's earlier green run.

**Post-merge flake investigation**: two of four full `bash scripts/verify.sh` runs against the fully-merged tree crashed inside `test-zruntime` - once in `HandlerPool exhaustion and recovery` (signal ABRT-class), once in `HandlerPool owned response survives pooled reuse` (signal TRAP) - different test, different signal each time. Before accepting this, ran a full A/B comparison: the crashing tests pre-date all five plans (present at `ae9615ae`); the identical `HandlerPool exhaustion and recovery` filter passed 8/8 in isolation and `test-zruntime` passed 13/13 standalone (0 crashes on either tree) - the crash only ever appeared inside a *full* `scripts/verify.sh` run, never a standalone `test-zruntime`/`zig build test` invocation; a pre-merge baseline worktree at `ae9615ae` ran clean on 3/3 full `verify.sh` + 6/6 `test-zruntime` + 1/1 aggregate `zig build test`, vs. the merged tree's 2/4 full `verify.sh` (13/13 clean standalone). This matches `build.zig`'s own documented, pre-existing class ("parallel duplicate roots have produced intermittent libc/JIT/arena teardown TRAPs on macOS") rather than a logic defect in any of the five fixes: different tests/signals crashing each time, zero reproduction in isolation, and a plausible mechanism (plans 011+012 add three new pool-lifecycle tests to `runtime_pool.zig`, increasing total HandlerPool create/destroy volume within one `test-zruntime` process invocation, which raises the odds of hitting this pre-existing macOS teardown race without being its cause). Not fully provably zero risk from the added test volume, but not a regression in the merged fixes' own correctness - each fix's isolated worktree verification, and this session's repeated isolated re-runs on the merged tree, are consistently green. Worth a dedicated investigation if it recurs; not blocking this merge.

| Plan | Status |
| --- | --- |
| 010 | DONE (merged `fc1bf1c8`, worktree commit `6bca740d`. Resumable `ChunkedBodyParseState`/`chunkedBodyConsumedResumable` added to `http_parser.zig`, threaded through `server.zig`'s `readRequestData`; `chunkedBodyConsumed` kept as a thin wrapper so `decodeChunkedBody`/`edge_server.zig` needed no changes. +3 unit tests (chunk-size-line/chunk-data/trailer split, via a "poisoned prefix" technique proving no rescanning) +1 real-socket integration test forcing a >16KB chunked body through multiple actual `posix.read()` calls. Red-proofed via compile-failure on revert (new identifiers undefined). Along the way found and fixed a real deadlock in the first draft of the integration test itself - writing a ~26KB payload synchronously into a macOS default 8KB `AF_UNIX` socket buffer before any reader ran; fixed by writing from a spawned thread. `test-server` (360/362, 2 skipped) + `test-zruntime` + `scripts/verify.sh` green on both the isolated worktree and the merged tree.) |
| 011 | DONE (merged `b7f36815`, worktree commit `dc3f1726`. Added `PoolingThresholds.max_dynamic_atoms: u32 = 32_768` (half the 65,534 `AtomTable.intern` hard cap) and changed the `.reuse_unbounded` arm of `HandlerPool.releaseForRequest`'s `policy_recycle` switch from unconditional `false` to `rt.ctx.atoms.count() >= self.pooling_thresholds.max_dynamic_atoms`, reusing the existing `dropRuntime` recycle path. `rt.ctx.atoms` turned out directly reachable (`Runtime.ctx: *Context`, no `user_data` cast needed) - simpler than the plan assumed. +2 regression tests (over-threshold recycles, under-threshold does not), red-proofed. Independently root-caused an unrelated `scripts/verify.sh` flake (`server_test.zig`'s hardcoded-50ms timeout test, timing-sensitive under CPU jitter) by reproducing it against unmodified baseline code with the fix stashed, confirming it pre-existing. `test-zts` + `test-zruntime` + `scripts/verify.sh` green on both the isolated worktree and the merged tree.) |
| 012 | DONE (merged `e323029a`, worktree commit `fc188fd6`. Added `decodeTag(comptime E, raw: u8) DeserializeError!E` using `std.enums.fromInt` (the plan's suggested `std.meta.intToEnum` doesn't exist in Zig 0.16.0) at the three confirmed-exhaustive-enum decode sites in `bytecode_cache.zig` (`ConstantTag`, `SpecialCode`, `PatternType`); confirmed `url_atom`'s `object.Atom` is non-exhaustive and correctly left untouched. Traced `server.zig`'s `ServerConfig.handler` switch to confirm the embedded/self-extracting-binary path genuinely has no source to recompile from (`handler_code` is a `""` placeholder), so `loadHandlerCached`'s embedded branch gets a clean-failure `catch` + log rather than an attempted recompile. +3 unit tests (one per exhaustive enum) +1 integration test (`HandlerPool.initWithEmbedded` with a corrupted `ConstantTag` byte in a real serialized blob), red-proofed. Noted but did not fix a pre-existing unrelated leak (`deserializePatternDispatch` missing an `errdefer` on its `allocator.create`) per surgical-changes discipline. Merge required manual conflict resolution: 011 and 012 both appended new `test {}` blocks after the same anchor test in `runtime_pool.zig` - resolved by keeping all three tests concatenated (011's two + 012's one). `test-zts` (1431->1448 across the two merges) + `test-zruntime` (382->383) + `scripts/verify.sh` green on both the isolated worktree and the merged tree.) |
| 013 | DONE (merged `146b246b`, worktree commit `a6bb6a73`. `property_diagnostics.zig` now imports `json_utils.zig` directly and calls `json_utils.writeJsonStringContent`; `system_rollout.zig` aliases `zts.json_utils.writeJsonString` (both already had a path to the canonical module, so no new cross-package import was needed). +1 regression test per file asserting a C0 control byte (0x01/0x1f) escapes correctly, red-proofed against the pre-fix local implementations. Confirmed no existing test asserted on the absence of that escaping. `test-zts` + `test-rollout` (84/84) + `scripts/verify.sh` green on both the isolated worktree and the merged tree; clean merge, no conflicts (neither file touched by intervening commits).) |
| 014 | DONE (merged `252d9b99`, worktree commit `c7ce54e9`. `compiler.zig:71` now calls `try JsParser.initFallible(...)` instead of the panicking `JsParser.init`. +1 `FailingAllocator`-based regression test in `compiler.zig`, red-proofed via compile-failure on revert. Hit the plan's anticipated STOP condition exactly: the new test wasn't collected by any suite (`root.zig`'s `pub const modules = @import(...)` is a lazily-analyzed, never-referenced top-level const, so the compiler never opens `modules/internal/compiler.zig` during `test-zts`) - resolved per the plan's pre-authorized fallback by adding a `test { _ = @import("modules/internal/compiler.zig"); }` anchor block in `root.zig`, mirroring the file's existing `opcode_parity.zig` anchor pattern; this pulled in `module_graph.zig` and siblings too (+16 tests total, not just 1). `test-zts` (1432->1448) + `test-zruntime` + `scripts/verify.sh` green on both the isolated worktree and the merged tree; clean merge, no conflicts.) |

## Dependency notes

- 002-005 each note `depends on 001` only so tests run against a clean format baseline; they are otherwise independent.
- 006-009 are independent of each other. Doing 001 + 007 first makes every other plan's verification trustworthy.
- 010-014 are independent of each other and of every plan above.

## Code-review follow-ups landed

- **SQL policy carries per-query operation end-to-end (code-review max, finding A)**: the runtime SQL allowlist distinguished db.read vs db.write only via `RuntimeSqlAllowList.queries` (operation-typed), but dev/serve (`ownDevPolicy`), deployed binaries (the `self_extract` wire format), and precompiled-to-`.zig` output all flattened the proven queries to an operation-agnostic name list (`.values`), so a query proven read-only also satisfied `db.write` (and vice-versa) in those modes - a mismatch with the contract's per-query proof. NOT exploitable (the runtime statement-shape guard in `sql.zig:207-211` enforces operation by the actually-registered statement, and a query name maps to one fixed statement), but a real inconsistency + misleading dead code. Fix: the wire format now serializes a per-query read-only flag; `ownDevPolicy` and the precompile codegen now emit `.queries` (with operation via the new `handler_policy.normalizedSqlQuery`); `build_command` dropped its flatten. `.values` is retained, documented, and used only by the name-only explicit-policy-file / JSON-checker skeleton surfaces (which have no operation data). +self_extract roundtrip now asserts the read/write split survives serialization (red-proven). All per-target suites + full scripts/verify.sh green, policy hash unchanged.

## Findings Surfaced During Execution (not yet planned)

- **Dormant uncollected tool tests (found executing 009) - RESOLVED**: `packages/tools/src/zts_cli.zig`, `describe_rule.zig`, and `search_rules.zig` had `test {}` blocks that **no test root collected** (those files are only reached through the `zts_cli` named module, never an `addTest` root - proven via an aggregate canary), and two `describe_rule.zig` tests used `std.io.fixedBufferStream` (removed in Zig 0.16) so they would not even compile. Fix: repaired the two tests to the 0.16 `std.Io.Writer.Allocating` + `.buffered()` idiom; added a `test-zts-cli` `addTest` root in `build.zig` (rooted at `zts_cli.zig`, mirroring `canonicalize_tests`) wired into the aggregate `test`; and added a `test { _ = describe_rule; _ = search_rules; }` reference anchor in `zts_cli.zig` (a plain `@import` does not pull a file's test blocks - the dev_cli.zig pattern / "CLI test-collection closure"). Red-proven: breaking the revived `writeUnknownRuleJson` test made `test-zts-cli` fail. test-zts-cli (75 pass) + full scripts/verify.sh green.

## Findings Considered And Not Planned In This Batch

- **Durable WAL write primitive swallows write errors (N5)**: `packages/zts/src/trace.zig:1744-1754` `writeAll` returns `void` and `break`s on write error, used by the fsync'd durable persist path. The oplog *does* fsync every step, but a failed write (ENOSPC/EIO) is invisible. Real robustness gap, MED confidence, small fix (an error-returning `writeAllChecked` for the durable call sites). Not planned this batch because it requires a disk-full/IO-error to manifest and is lower stakes than the trust-boundary and gate issues above.
- **JIT tier duplication (strategic)**: `optimized.zig` and `baseline.zig` reimplement 25+ `emit*` helpers (e.g. `emitForOfNext`, `emitComparison`, `emitConditionalJump`), the root cause of recurring "fix the JIT bug in both tiers" work. Already tracked by `DEFERRED_VM_LOOP_DEDUPE_PLAN.md` and gated; do not open a competing plan - extend that one when the gates are met. L effort, HIGH risk.
- **Two JSON-reading strategies (strategic)**: robust `contract_json_parser.zig` (`JsonParser`) vs substring `trace.findJson*` scanners wired into the replay/mock/test path; they have drifted and produced the same bug class fixed twice. M effort, MED risk. Consolidation candidate, deferred.
- **`zruntime.zig` god-module (strategic)**: 7.5 KLOC, largest file in the repo, on the request hot path; continue the established alias-in-place sibling-extraction pattern (fetch/response construction, header KV). M-L effort, MED risk; ongoing.
- **`type_pool.zig` truncation risks**: fixed parser buffers and `u16` casts. Needs a design decision (fail-closed on oversize types vs dynamic backing) before a plan. (Carried over from the `f2c637d` pass.)
- **Server accept-path test coverage**: health/readiness/keep-alive/shutdown. Worth adding; the panic-isolation E2E is green so this is lower priority. (Carried over.)

## Findings Surfaced This Pass, Not Planned (2026-07-02, verified but lower leverage)

- **`JSObject.create` leaks the object if overflow-slot allocation fails** (`packages/zts/src/object.zig:1373-1393`): no `errdefer` on the `allocator.create(JSObject)` at line 1374 before the fallible `ensureOverflowCapacity` call at line 1390. Real but low severity (leak only on the OOM path, for shapes with >8 properties); S effort, LOW risk. Small enough to fold into plan 011 or 012's executor's session as a drive-by if convenient, but not planned as its own item this batch.
- **SQLite unconditionally linked into every deploy binary** (`packages/zts/build.zig:29-39`; confirmed via `nm` on `zig-out/bin/zttp-runtime`, ~270 `sqlite3_*` symbols, 22-23 MB binary): tension with the stated "small binary"/"zero dependencies" goals, but `zttp:sql` is a documented first-class built-in, so this may be an accepted tradeoff rather than an oversight. Needs a product decision (gate by contract-derived SQL usage vs. accept as-is) before it's plannable; M effort, MED risk (touches build graph + deploy-artifact assembly).
- **Global per-file inline-cache budget (512 slots, not reset per function)** (`packages/zts/src/parser/codegen.zig:2801-2825`, `IC_CACHE_SIZE = 512` at `:116`): silent perf cliff for handler files with >512 property-access sites — falls back to uncached field access with no diagnostic. Mechanism confirmed; real-world trigger frequency on actual handler files not measured. M effort.
- **Hidden-class shape rebuild is O(n) work repeated on every property added past 8** (`packages/zts/src/object.zig:1155-1190`, `buildSortedProperties` re-sorts the full cumulative list on every transition once `new_prop_count >= 8`): confirmed mechanism; real trigger frequency on precompiled object-literal-shaped handlers (which bypass this via `set_slot`) uncertain. M effort, MED confidence on impact.
- **`JSObject` field order wastes ~16 bytes/instance** (`packages/zts/src/object.zig:1304-1315`, `extern struct`, pointers interleaved with `u8`/`u16` fields): structurally sound reorder (120 -> 104 bytes), not independently compiled/measured this pass. S effort, MED risk only because `extern struct` field order may be relied on by JIT-emitted fixed-offset code elsewhere — audit offset-dependent sites before reordering.
- **`server.zig` god-module** (3325 lines; confirmed struct spans: `ConnectionPool` 295-1220, `Server` 1469-2313, `StaticFileCache` 2386-2610): `ConnectionPool` and `StaticFileCache` are already self-contained structs and close to a mechanical extraction. L effort given the file's centrality to the request path; worth a dedicated plan when there's appetite for a maintainability-only pass, not bundled here with the five availability/correctness fixes.
- **`zruntime.zig` `Runtime` (6813 lines) and `precompile.zig` (4754 lines) god-files**: mix engine binding/HTTP lifecycle/durable-run state/WS-queue refs (zruntime), and orchestration/contract assembly/SQL validation/codegen (precompile) respectively. `zruntime.zig` specifically is already tracked as an ongoing target via the established alias-in-place sibling-extraction pattern (see "Surgical monolith split" history) — extend that effort rather than opening a competing plan. L effort each.
- **`containsString`-style helper reimplemented ~5 times** across packages that already share a dependency graph: real duplication, trivial consolidation, but no drift/correctness bug found in any copy (unlike the JSON-escaping duplication in plan 013, which does have a drift bug) — pure tidiness. S effort, low urgency.
- **Module-binding validator logic duplicated between SDK and internal registry** (`packages/zttp-sdk/src/binding.zig:193` vs `packages/zts/src/module_binding.zig:1619`, both independently implementing the same invariant checks over structurally-parallel types): a comptime assertion already keeps the two `ModuleBinding` *shapes* aligned; nothing keeps the two *validators* behaviorally aligned if a rule is added to one and forgotten in the other. S effort, LOW risk (comptime-only code). No evidence of current drift, only future-drift risk.
- **Naming collisions: `proof_cli.zig`/`proofs_cli.zig`, and `Runtime` (zts package vs. runtime package)**: purely cosmetic/DX, flagged independently by two separate audit passes (signal that it's a genuine live source of misreads). S effort, LOW risk, pure rename — good "if someone's already touching those files" opportunistic fix, not worth a dedicated plan/review cycle on its own.
- **`zttp:crypto` HMAC/base64 and `zttp:sql` execution path effectively untested**: `hmacSha256`/`base64Decode` have no executing test (only a dead-code caller in `examples/modules/modules.ts`); `zttp:sql`'s param binding/integer-precision guard/row conversion is only type-checked, never executed against a real query. Security-adjacent, so normally floats up, but this is a test-coverage gap rather than a demonstrated defect — recommend as the next test-writing pass rather than a remediation plan (nothing to "fix" besides adding tests, which doesn't fit this batch's bug-fix framing well). M effort.
- **Registering one built-in module requires lockstep edits across up to 7 list sites** (`packages/modules/src/root.zig` + `packages/zts/src/builtin_modules.zig`): already comptime-guarded against silent drift (a comptime block catches length/specifier mismatch), so this is pure maintenance friction, not a correctness risk. M effort to consolidate via `std.meta.fields` comptime iteration; low urgency given the existing guard.
- **`zts/src/http.zig` mixes HTTP Request/Response, JSX/SSR, generic JSON serialization, and HAL/hypermedia resource building in one 2025-line file**: a real second god-file beyond the already-known largest-files list, but a 3-way split (`http.zig`/`jsx.zig`/`hypermedia.zig`) needs care around existing `zts.http.*` call sites; bundling with the `runtime_http.zig` naming-collision finding below into one future "naming + file-boundary cleanup" pass makes more sense than planning in isolation now.
- **`runtime_http.zig` name collides conceptually with `zts/src/http.zig`** (outbound fetch bridge vs. inbound JS-facing Request/Response — unrelated concerns sharing the "http" name across three packages): cosmetic, pairs naturally with the `http.zig` split above if that's ever planned.

## Findings Refuted This Pass (do not re-raise without new evidence)

- **"Durable writes never fsync"**: REFUTED. The oplog WAL (`packages/zts/src/trace.zig` `DurableState`) calls `fsyncFd(self.oplog_fd)` after every persist (`persistStepStart/Result`, `persistWaitSignal`, `markComplete`, lines ~1380-1535). The durability guarantee holds; only the residual write-error-swallowing (N5 above) remains.
- **`fetchWithRetry` return metadata**: REFUTED. `packages/modules/src/net/fetch.zig:48` correctly declares `.object`. Only `zttp:io`'s `parallel`/`race` are wrong (plan 008).
- **`time.zig` ISO parse/arithmetic**: REFUTED. `parseIso` range-checks month/day/clock and handles leap years (`:110-113,127,134`); `addSeconds`/`formatIso`/`formatHttp` saturate on overflow and clamp to a max epoch. The validation gap (plan 006) is in `validate.zig` only.

## Standard Completion Gate

After any plan is executed, run at least:

```sh
zig fmt --check build.zig packages/
bash scripts/verify.sh        # once plan 007 lands; until then: zig build test
git diff --check
git status --short
```

Add the narrower commands from the individual plan before the aggregate.
