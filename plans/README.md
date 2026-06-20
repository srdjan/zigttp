# Advisory Plan Index

Originally planned at commit: `f2c637d` (`refactor(runtime): group self-extract payload params into PayloadInput`).
Re-validated and extended at commit: `560239d` (`feat(expert,cli): inline approval diff, ...`) on 2026-06-20.

This directory contains source-backed remediation plans from read-only advisor passes. The advisor did not modify production source. Each plan is self-contained for a fresh-context executor.

## Reconciliation note (pass at `560239d`)

Plans 001-005 were planned at `f2c637d`. The only commit since (`560239d`) touched `packages/pi/src/{loop,repl}.zig` and `packages/runtime/src/{dev_cli,init_command}.zig` — none of which any of 001-005 cite — so **all five remain valid at `560239d`** (their drift checks against `f2c637d` are still empty for their in-scope files).

- **001 (format baseline)**: the fix already exists in the working tree, uncommitted. The `M packages/runtime/src/zruntime.zig` change is exactly the formatting-only edit this plan prescribes (collapsing two double-blank-lines), and `zig fmt --check build.zig packages/` passes on the working tree. A clean checkout of `560239d` still fails the gate because the fix is not committed. Local toolchain is Zig 0.16.0 stable (matches the project baseline), so the failure was a real format issue, not nightly drift. Closing 001 = commit that formatting-only change.

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
| 6 | [008-fix-io-module-return-metadata.md](008-fix-io-module-return-metadata.md) | P2 | S | LOW | `zigttp:io` `parallel`/`race` declare `.string` but return array/Response; misleads type checker, `.d.ts`, expert. |
| 7 | [009-reject-unknown-cli-flags.md](009-reject-unknown-cli-flags.md) | P2 | S | LOW-MED | `features`/`modules`/`meta`/`describe-rule`/`search` silently drop unknown flags. |
| 8 | [004-correct-sparse-array-callbacks.md](004-correct-sparse-array-callbacks.md) | P2 | M | MED | Sparse-array callbacks have observable JS semantics gaps. |
| 9 | [005-use-utf16-string-indexing.md](005-use-utf16-string-indexing.md) | P2 | M | MED | Non-ASCII string indexing is byte-based in scoped methods. |

Status values below: TODO | IN PROGRESS | DONE | BLOCKED (<reason>) | REJECTED (<reason>)

| Plan | Status |
| --- | --- |
| 001 | DONE (formatting-only fix committed; `zig fmt --check` + test-zruntime/test-server green) |
| 002 | TODO |
| 003 | TODO |
| 004 | TODO |
| 005 | TODO |
| 006 | DONE (validateIsoDate/Datetime now range-check month/day (leap-aware via pub time.daysInMonth), clock fields, and bound the trailing fractional+tz grammar; +9 regression cases; test-modules + zig build test green, fmt clean) |
| 007 | DONE (scripts/verify.sh mirrors ci.yml test job — 9 steps + format note; CLAUDE.md test claim corrected; full `bash scripts/verify.sh` green, exit 0) |
| 008 | TODO |
| 009 | TODO |

## Dependency notes

- 002-005 each note `depends on 001` only so tests run against a clean format baseline; they are otherwise independent.
- 006-009 are independent of each other. Doing 001 + 007 first makes every other plan's verification trustworthy.

## Findings Considered And Not Planned In This Batch

- **Durable WAL write primitive swallows write errors (N5)**: `packages/zigts/src/trace.zig:1744-1754` `writeAll` returns `void` and `break`s on write error, used by the fsync'd durable persist path. The oplog *does* fsync every step, but a failed write (ENOSPC/EIO) is invisible. Real robustness gap, MED confidence, small fix (an error-returning `writeAllChecked` for the durable call sites). Not planned this batch because it requires a disk-full/IO-error to manifest and is lower stakes than the trust-boundary and gate issues above.
- **JIT tier duplication (strategic)**: `optimized.zig` and `baseline.zig` reimplement 25+ `emit*` helpers (e.g. `emitForOfNext`, `emitComparison`, `emitConditionalJump`), the root cause of recurring "fix the JIT bug in both tiers" work. Already tracked by `DEFERRED_VM_LOOP_DEDUPE_PLAN.md` and gated; do not open a competing plan — extend that one when the gates are met. L effort, HIGH risk.
- **Two JSON-reading strategies (strategic)**: robust `contract_json_parser.zig` (`JsonParser`) vs substring `trace.findJson*` scanners wired into the replay/mock/test path; they have drifted and produced the same bug class fixed twice. M effort, MED risk. Consolidation candidate, deferred.
- **`zruntime.zig` god-module (strategic)**: 7.5 KLOC, largest file in the repo, on the request hot path; continue the established alias-in-place sibling-extraction pattern (fetch/response construction, header KV). M-L effort, MED risk; ongoing.
- **`type_pool.zig` truncation risks**: fixed parser buffers and `u16` casts. Needs a design decision (fail-closed on oversize types vs dynamic backing) before a plan. (Carried over from the `f2c637d` pass.)
- **Server accept-path test coverage**: health/readiness/keep-alive/shutdown. Worth adding; the panic-isolation E2E is green so this is lower priority. (Carried over.)

## Findings Refuted This Pass (do not re-raise without new evidence)

- **"Durable writes never fsync"**: REFUTED. The oplog WAL (`packages/zigts/src/trace.zig` `DurableState`) calls `fsyncFd(self.oplog_fd)` after every persist (`persistStepStart/Result`, `persistWaitSignal`, `markComplete`, lines ~1380-1535). The durability guarantee holds; only the residual write-error-swallowing (N5 above) remains.
- **`fetchWithRetry` return metadata**: REFUTED. `packages/modules/src/net/fetch.zig:48` correctly declares `.object`. Only `zigttp:io`'s `parallel`/`race` are wrong (plan 008).
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
