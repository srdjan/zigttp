# Plan 014: Use the fallible parser constructor in the multi-file module compiler

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only a4a731bd -- packages/zts/src/modules/internal/compiler.zig packages/zts/src/parser/parse.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P2
- **Effort**: S
- **Risk**: LOW
- **Depends on**: none
- **Category**: bug (availability)
- **Planned at**: commit `a4a731bd` and 2026-07-02

## Why this matters

`ModuleCompiler.compileAll` (the multi-file `import`-graph compiler, used whenever a handler imports another local file, including during `zttp dev` live-reload) is otherwise careful: it sets up `errdefer` cleanup for its parser/codegen lists specifically so a later error unwinds cleanly, and its caller in `zruntime.zig` wraps the whole call in `catch |err| { log; return err; }`. But it constructs each per-module parser with `zts_parser.JsParser.init(...)`, which is defined as `initFallible(...) catch unreachable` — a panic, not an error, on the one fallible step inside `initFallible` (an allocation in `ScopeAnalyzer.initFallible`). A panic here bypasses every `errdefer` in `compileAll` and the caller's `catch` entirely, aborting the process. `docs/internals/architecture.md` documents that `zttp dev` "Compilation failures keep the currently serving handler active" — an OOM here breaks that guarantee by crashing the whole dev server instead of just failing the one reload.

## Current state

- `packages/zts/src/modules/internal/compiler.zig:71`:
  ```zig
  var js_parser = zts_parser.JsParser.init(self.allocator, source);
  ```
  inside `pub fn compileAll(self: *ModuleCompiler, graph: *module_graph.ModuleGraph) !CompileResult` (`compiler.zig:47-90`), which already has `errdefer` blocks at `compiler.zig:52, 56-59, 61-64` for the `compiled`/`parsers`/`codegens` lists.
- `packages/zts/src/parser/parse.zig:113-115`:
  ```zig
  pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
      return initFallible(allocator, source) catch unreachable;
  }
  ```
- `packages/zts/src/parser/parse.zig:117-135` — `pub fn initFallible(allocator: std.mem.Allocator, source: []const u8) !Parser`, the non-panicking version, already exported and already used elsewhere in the codebase (the version that should be called instead).
- `packages/runtime/src/zruntime.zig:753-788` — `compileAndRunFileImports`, the sole caller of `ModuleCompiler.compileAll` in the codebase (confirmed: `packages/zts/src/modules/root.zig:34` only re-exports the type; `zruntime.zig:784` is the only call site). The call already has a `catch |err| { std.log.err(...); return err; }` wrapper (`zruntime.zig:784-787`) that a real error from `compileAll` would flow into cleanly — it is only the `unreachable`-panic inside `JsParser.init` that defeats this.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Build | `zig build` | exit 0 |
| zts tests | `zig build test-zts` | success |
| ZRuntime tests (compileAndRunFileImports caller) | `zig build test-zruntime` | success |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Full local gate | `bash scripts/verify.sh` | exit 0 |

## Scope

**In scope, the only files/directories to modify:**

- `packages/zts/src/modules/internal/compiler.zig` — change line 71 from `JsParser.init(...)` to `try JsParser.initFallible(...)`, and adjust the surrounding code as needed so the function still returns `!CompileResult` correctly on this new error path (it already does — `compileAll`'s signature is already `!CompileResult`).

**Out of scope:**

- `packages/zts/src/parser/parse.zig` — `Parser.init`/`initFallible` themselves are correct as written and used intentionally elsewhere (the panicking `init` is a legitimate convenience wrapper for call sites that have already made allocation failure fatal by design, e.g. top-level CLI entry points). Do not remove or change `Parser.init`.
- Any other caller of `Parser.init` in the codebase — this plan only touches the one call site inside `ModuleCompiler.compileAll` that sits behind an `errdefer`/`catch` chain that a panic defeats. Do not sweep other `JsParser.init` call sites without separately verifying each one's caller actually expects a recoverable error (some may be legitimately fine as-is).
- `zruntime.zig:753-788` (`compileAndRunFileImports`) — its existing `catch` is already correct; no change needed there once the panic is removed at the source.

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(zts): stop the module compiler panicking on parser-init allocation failure`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Switch to the fallible constructor

In `compiler.zig:71`, change:
```zig
var js_parser = zts_parser.JsParser.init(self.allocator, source);
```
to:
```zig
var js_parser = try zts_parser.JsParser.initFallible(self.allocator, source);
```
Confirm this doesn't change any other observable behavior of `compileAll` for the non-error case (the return type of `initFallible` is `!Parser`, same `Parser` value as `init` returns on success, just wrapped in an error union).

**Verify**: `zig build test-zts test-zruntime` -> success.

### Step 2: Add a regression test

Add a test that drives `ModuleCompiler.compileAll` (or `compileAndRunFileImports`, whichever is more directly testable with a controlled allocator) with a `std.testing.FailingAllocator` configured to fail on the allocation inside `Parser.initFallible`, and assert that `compileAll` returns an error rather than the test process aborting. Model the failing-allocator harness after existing tests in this codebase that already use `std.testing.FailingAllocator` (search for it in `zruntime.zig` or `context.zig` for the established pattern per this repo's testing conventions).

**Verify**: `zig build test-zts` (or `test-zruntime`, matching wherever the test lands) -> success; the new test must fail (panic) against the pre-fix code (temporarily revert Step 1 locally to confirm, then reapply) — do not skip this red-proof.

## Test plan

- New test exercising `ModuleCompiler.compileAll` under a `FailingAllocator` tuned to fail during parser construction, asserting a clean error return.
- Regression: proven to panic on the pre-fix code, pass after the fix (Step 2 red-proof).
- Final: `bash scripts/verify.sh` -> exit 0.

## Done criteria

All must hold:

- [ ] `compiler.zig:71` calls `JsParser.initFallible` via `try`, not the panicking `JsParser.init`.
- [ ] A multi-file-import compile that fails to allocate during parser construction returns a normal Zig error, observable by `compileAndRunFileImports`'s existing `catch`, instead of aborting the process.
- [ ] `zig build test-zts test-zruntime` exit 0.
- [ ] `bash scripts/verify.sh` exit 0.
- [ ] New test proves the fix, red-proven against the pre-fix panic.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match the live code (re-check line numbers first).
- The new test's file (`compiler.zig` has zero existing `test` blocks today) turns out not to be collected by any `zig build test-*` step, per this repo's known "CLI test-collection closure" gotcha (a file reached only via a named-module boundary, not a relative `@import` chain from a test root, can have its `test {}` blocks silently uncollected). If your new test doesn't appear in any test runner's output/count, check whether `packages/zts/src/root.zig` (the `test-zts` root) actually reaches `modules/internal/compiler.zig` transitively; if not, add a `test { _ = @import("modules/internal/compiler.zig"); }` anchor block in a file that root.zig's test root does reach (mirroring the fix already applied for `zts_cli.zig`/`describe_rule.zig`/`search_rules.zig`, see `plans/README.md`'s "Findings Surfaced During Execution" note), and report that you did so.
- A step's verification fails twice after reasonable local correction.

## Maintenance notes

- This is a narrow, single-call-site fix. If a future audit wants to sweep all `Parser.init` call sites for the same panic-vs-error question, that should be a separate plan, since each caller needs its own reachability/blast-radius check (some may be genuinely fine as fatal-on-OOM by design).
