# Simplification & Footprint Proposal

Status: proposal for review. Nothing here is implemented. Approve individual
items from the action list (section 5); only approved items get built.

## Framing

zigttp is ~226 KLOC of Zig across 8 packages at v0.1.0-beta. Performance targets
are already met (README: ~3.5ms cold-start floor, 4.8MB binary, ~13MB baseline),
and the house rule is "benchmark before optimizing; if targets met, stop." This
proposal therefore targets **footprint and complexity**, not cycle-shaving. Any
item that touches a hot path is gated on a `../zigttp-bench` run showing **no
regression**, never on chasing a speculative gain.

Two things are explicitly out of scope this round (decided): the `pi` expert
agent stays in core, and tier-2 JIT (`jit/optimized.zig`) is left as-is.

## 1. Behavior-preserving simplifications (in place)

### 1.1 Remove dead scaffolding (P1, trivial, zero risk)
- `site/` is an empty directory. Remove it.
- `packages/zigttp-ext-demo/` is an empty package (0 `.zig` files). Remove it,
  or restore its intended contents if it is a stub for a planned demo.
- Verify: `grep -rn "zigttp-ext-demo\|zigttp_ext_demo" build.zig packages/*/build.zig`
  returns nothing before deletion; `zig build` still succeeds after.

### 1.2 Repo hygiene for tracked binaries (P2, trivial)
- `docs/pi-tui-demo.mov` (186KB) is git-tracked. Move it to a GitHub Release
  asset (or the website static dir) and replace with a link in the docs.
- `demo/demo.cast` (7KB, asciinema text) is fine to keep.
- No action on `codedb.snapshot` (14MB): already gitignored, local only.

### 1.3 Coalesce duplicated request/response tails in `zruntime.zig` (P3, medium risk, bench-gated)
- `createRequestObject` (`packages/runtime/src/zruntime.zig:2028`) takes a shaped
  fast path and delegates to `createRequestObjectDynamic` (:2091) as a fallback.
  `extractResponseInternal` (:2171) has the same shaped (`:2192`) vs dynamic
  split.
- The two halves of each pair share a common tail (header-loop string creation,
  body-string handling). Only the object-construction prologue differs (direct
  slot writes vs property sets). The dedup target is the **shared tail**, not the
  whole function: the shaped prologue must stay for slot-access speed.
- Honest caveat: `http_shapes` is on by default, so the dynamic path is rarely
  taken at runtime but always compiled. The win is ~60-100 LOC and one fewer
  divergent code path to maintain, not speed.
- Verify: `zig build test-zruntime` + `bash scripts/test-examples.sh` + a
  `../zigttp-bench` cold-start/throughput run that must match baseline.

### 1.4 Catalog `ProofLevel` duplication (P3, document-only)
- `pub const ProofLevel` is defined in **four** files: `contract_diff.zig`,
  `system_linker.zig` (both in `packages/zigts/src/`),
  `packages/tools/src/deploy_manifest.zig`, and
  `packages/proof-review/src/review.zig`.
- The `proof-review` copy is intentional (keeps that package independent of
  zigts types). The other three are candidates to consolidate into one canonical
  definition in `contract_diff.zig` that `system_linker` and `deploy_manifest`
  import.
- Verdict: low urgency. Worth doing only alongside an unrelated edit to those
  files; not a standalone task. Listed for completeness.

## 2. Opt-in compilation (build-graph footprint)

Goal: shrink the **default** `zigttp` / `zigts` binaries by gating advanced
subsystems behind `build.zig` options, with **no loss of functionality** (the
flag restores them). Each entry names the command(s) that become flag-gated.

### 2.1 Studio (P2) - mostly done, finish it
- `packages/runtime/src/studio.zig` is 1585 LOC; `studio_stub.zig` (139 LOC) is
  the no-op default. Already conditionally compiled via `enable_studio` in
  `packages/runtime/build.zig` (false for the `zigttp-runtime` template at :44,
  **true for the `zigttp` dev CLI** at :48).
- Proposal: expose `enable_studio` as a top-level `-Dstudio` build option
  defaulting to **false** for the dev CLI too. Default `zigttp` then drops 1585
  LOC; `zig build -Dstudio` restores `zigttp studio`. This matches the scope
  doc's "hidden in v0.1.0-beta" stance.

### 2.2 Edge server (P2)
- `packages/runtime/src/edge_server.zig` (914 LOC) backs the real `zigttp edge`
  command (dispatched at `dev_cli.zig:201` -> `runtime_cli.edgeCommand`). It is
  not dead, just advanced (help --all only).
- Proposal: gate behind `-Dedge` (default off) with a stub that prints
  "rebuild with -Dedge" like the studio stub pattern. Default binary drops
  914 LOC.

### 2.3 IDE / peripheral analyzers - NOT DONE (investigated, declined)
Original idea: one `-Dide-tools` flag gating `rollout` (`system_rollout.zig`,
1224 LOC), `edit-simulate` (`edit_simulate.zig`, 468 LOC), `canonicalize`
(`canonicalize.zig`, 1105 LOC), and `gen-tests`. Investigation changed this:

- **canonicalize + edit_simulate cannot be gated without losing functionality.**
  The `pi` expert agent calls them in-process as tools:
  `packages/pi/src/tools/zigts_expert_canonicalize.zig` uses
  `canonicalize.collect / simulateRefactors / writeJsonWithSimulation`, and
  `zigts_expert_edit_simulate.zig` uses `edit_simulate`. Both reach them through
  the shared `zigts_cli` module. Gating these out by default would degrade the
  expert agent - a functionality loss, which is out of bounds.
- **gen-tests is entangled with the core `precompile.zig`** (4557 LOC, always
  compiled for `check`/`compile`). Gating its dispatch yields ~0 footprint win.
- **system_rollout (1224 LOC) is the only cleanly gatable one** (private to
  `zigts_cli` dispatch, no expert coupling). But `zigts_cli` is consumed from 5
  `b.dependency("zigttp_tools", ...)` sites across 4 packages, and `pi_app` is
  linked into `zigttp` via the runtime. The repo already threads `perf_histogram`
  through ~25 touchpoints precisely to keep dependency option-sets identical;
  the in-code comments warn that a divergent option-set "instantiates [the dep]
  twice, splitting type identity." Adding a `rollout` option means replicating
  that fragile threading discipline across ~9 sites for one advanced command.
- **Verdict: declined.** Disproportionate build-graph risk for 1224 LOC of an
  advanced command, and the larger share (canonicalize/edit_simulate) is not
  gatable at all. Recommend leaving these in core. Revisit only if `pi` is later
  extracted (then canonicalize/edit_simulate decouple and the threading shrinks).

### 2.4 WASM playground (P3, low impact)
- `packages/zigts/src/wasm/` is only 101 LOC (4 files) plus
  `scripts/build-wasm-playground.sh`. It compiles only under the existing
  `wasm`/`analyzer_only` target, so it does **not** bloat the native binary.
- Proposal: no build-flag change needed. Optional: move the 4 files +
  build script to a satellite `zigttp-wasm-playground` repo if you want them out
  of the engine tree. Low value; listed for completeness.

## 3. Explicitly excluded (with rationale)

- **`pi` expert agent (~37 KLOC)** stays in core. It is the single largest
  extraction lever for a future `zigttp-expert` repo, but it has ~95 in-process
  import edges into `zigts` (26 expert tools call analyzers directly). Cleanly
  *deletable from a build* (already compiled only into `zigttp`, never into
  `zigts` or `zigttp-runtime`) but not cleanly *separable to its own repo*
  without first defining a stable analyzer seam (plugin or IPC). Not attempted
  now; flagged for a future decision.
- **Tier-2 JIT** (`jit/optimized.zig`, 2530 LOC + promotion in
  `interpreter/jit_compile.zig`) left as-is. It is already runtime-gated via
  `jit_policy` env vars and documented as opt-in/unstable in the scope doc.

## 4. What is NOT a problem (verified)

- `codedb.snapshot` (14MB) is already gitignored. No action.
- The large core analyzer files (`contract_builder.zig` 3630,
  `handler_verifier.zig`, `flow_checker.zig`, `type_checker.zig`,
  `bool_checker.zig`, `spec_discharge.zig`, `module_binding.zig`) **are** the
  product. Do not touch for size; they are cohesive single-pass passes.
- The four agent entry points in `pi` (repl, tui, rpc, autoloop) funnel through
  one shared `loop.runTurnWith` core - no duplicated agent-loop logic.

## 5. Prioritized action list

| # | Item | Section | Status | Result |
|---|------|---------|--------|--------|
| 1 | Remove empty `site/` + `zigttp-ext-demo/` | 1.1 | DONE | both removed, `zig build` green |
| 3 | `-Dstudio` default-off for dev CLI (−1585 LOC) | 2.1 | DONE | default excludes studio; `-Dstudio` restores; cmd reports cleanly |
| 4 | `-Dedge` gate (−914 LOC) | 2.2 | DONE | default excludes edge; `-Dedge` restores; cmd reports cleanly |
| 2 | `-Dide-tools` gate | 2.3 | DECLINED | expert-coupled (canon/edit-sim) + fragile build threading (rollout) |
| 5 | Move `pi-tui-demo.mov` out of git | 1.2 | TODO | low priority |
| 6 | Coalesce zruntime shaped/dynamic tails | 1.3 | TODO | bench-gated, opportunistic |
| 7 | Consolidate `ProofLevel` (3 of 4 copies) | 1.4 | TODO | opportunistic |

Default-binary footprint reduction landed (items 3+4): ~2500 LOC of advanced
subsystems (studio + edge) moved behind opt-in flags, full functionality
preserved via `-Dstudio` / `-Dedge`.

## 6. CI note (from items 3/4)

`zig build test` now runs against the stubs by default, so the internal test
blocks in `studio.zig` and `edge_server.zig` are not compiled in the default
test run (CLI test count: 440 default vs 465 with both flags). To keep covering
those modules, CI should add a `zig build test -Dstudio -Dedge` step alongside
the default `zig build test`.

## 7. Status

Items 1, 3, 4 implemented and verified (full `zig build test` green; both
`-Dstudio`/`-Dedge` on and off build and pass). Item 2 investigated and declined
with rationale in section 2.3. Items 5-7 remain optional follow-ups.
