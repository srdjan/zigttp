# Plan 013: Replace the two drifted JSON-string-escaping copies with the canonical one

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only a4a731bd -- packages/zts/src/property_diagnostics.zig packages/tools/src/system_rollout.zig packages/zts/src/json_utils.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P1
- **Effort**: S
- **Risk**: LOW
- **Depends on**: none
- **Category**: bug
- **Planned at**: commit `a4a731bd` and 2026-07-02

## Why this matters

`packages/zts/src/json_utils.zig` is the canonical, shared JSON-string-escaping helper (already reused by `handler_contract.zig`, `security_events.zig`, `deploy_manifest.zig`, `openapi_manifest.zig`, and `manifest_alignment.zig`). It escapes `"`, `\`, `\n`, `\r`, `\t`, and all other C0 control characters (`0x00-0x08`, `0x0B-0x0C`, `0x0E-0x1F`) as `\u00XX`. Two other files independently reimplement the same logic and both are missing the C0-control-character branch:

- `packages/zts/src/property_diagnostics.zig:372-383`
- `packages/tools/src/system_rollout.zig:863-876`

Any string routed through either local copy that contains a raw control byte outside `\n`/`\r`/`\t` is written unescaped into JSON output, producing output that violates RFC 8259 and will fail strict JSON parsers downstream. The two copies also silently diverge further from the canonical version on any future fix to it (e.g. if `json_utils.zig` ever adds surrogate-pair or `/` escaping, these two will not get it).

## Current state

- Canonical implementation: `packages/zts/src/json_utils.zig:6-26`:
  ```zig
  pub fn writeJsonStringContent(writer: anytype, s: []const u8) !void {
      for (s) |c| {
          switch (c) {
              '"' => try writer.writeAll("\\\""),
              '\\' => try writer.writeAll("\\\\"),
              '\n' => try writer.writeAll("\\n"),
              '\r' => try writer.writeAll("\\r"),
              '\t' => try writer.writeAll("\\t"),
              0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
                  try writer.print("\\u{x:0>4}", .{@as(u16, c)});
              },
              else => try writer.writeByte(c),
          }
      }
  }

  pub fn writeJsonString(writer: anytype, s: []const u8) !void {
      try writer.writeByte('"');
      try writeJsonStringContent(writer, s);
      try writer.writeByte('"');
  }
  ```
- Already-correct reuse pattern to follow: `packages/zts/src/handler_contract.zig:108-109`:
  ```zig
  pub const writeJsonStringContent = json_utils.writeJsonStringContent;
  pub const writeJsonString = json_utils.writeJsonString;
  ```
- `packages/zts/src/root.zig:96` — `pub const json_utils = @import("json_utils.zig");` (public, so any package that imports the `zts` module, not just files inside the `zts` package, can reach it as `zts.json_utils.writeJsonString`/`writeJsonStringContent`).
- Drifted copy 1: `packages/zts/src/property_diagnostics.zig:372-383` — a local `fn writeJsonStringContent` missing the `0x00...0x08, 0x0b...0x0c, 0x0e...0x1f` branch. Single call site: `property_diagnostics.zig:330`, `try writeJsonStringContent(writer, v.message);`. This file already imports `handler_contract.zig` at `property_diagnostics.zig:22` (`const handler_contract = @import("handler_contract.zig");`), but the cleaner fix is a direct import of `json_utils.zig` (same package, sibling file) rather than routing through the `handler_contract` re-export.
- Drifted copy 2: `packages/tools/src/system_rollout.zig:863-876` — a local `fn writeJsonString` (note: this one wraps quote bytes too, matching `json_utils.writeJsonString`'s signature exactly, not `writeJsonStringContent`'s), missing the same control-character branch. Eight call sites: `system_rollout.zig:742, 747, 754, 762, 775, 782, 793, 800`. This file already imports the `zts` module and re-exports pieces of it (`system_rollout.zig:1-6`: `const zts = @import("zts"); ... const handler_contract = zts.handler_contract;`), and `zts.json_utils` is directly reachable through that same `zts` import per `root.zig:96` — no new import declaration is needed, just a new local alias or direct qualified calls.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Build | `zig build` | exit 0 |
| zts tests (property_diagnostics, json_utils) | `zig build test-zts` | success |
| Tools rollout tests (system_rollout) | `zig build test-rollout` | success |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Full local gate | `bash scripts/verify.sh` | exit 0 |

## Scope

**In scope, the only files/directories to modify:**

- `packages/zts/src/property_diagnostics.zig` — delete the local `writeJsonStringContent` (`372-383`), add `const json_utils = @import("json_utils.zig");` near the existing imports (`16-22`), update the one call site (`330`) to `json_utils.writeJsonStringContent(...)`.
- `packages/tools/src/system_rollout.zig` — delete the local `writeJsonString` (`863-876`), add a local alias `const writeJsonString = zts.json_utils.writeJsonString;` near the existing `handler_contract`/`system_linker` aliases (`system_rollout.zig:4-6`), so the eight existing call sites (`742, 747, 754, 762, 775, 782, 793, 800`) keep working unchanged.

**Out of scope:**

- `packages/zts/src/json_utils.zig` itself — canonical, not being changed.
- Any other file already using `json_utils.writeJsonStringContent`/`writeJsonString` directly or via the `handler_contract` alias (`handler_contract.zig`, `security_events.zig`, `deploy_manifest.zig`, `openapi_manifest.zig`, `manifest_alignment.zig`) — already correct, do not touch.
- Any behavior change to what gets escaped beyond making the two drifted copies match the canonical version (i.e. do not change `json_utils.zig`'s escaping rules as part of this plan).

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(zts,tools): stop two local JSON-escaping copies from silently emitting invalid JSON`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Fix `property_diagnostics.zig`

Add the `json_utils` import, delete the local `writeJsonStringContent` function, update the call site at line 330 to use `json_utils.writeJsonStringContent`.

**Verify**: `zig build test-zts` -> success. Existing tests `property_diagnostics.zig:457, 468, 494, 553` must still pass unmodified.

### Step 2: Fix `system_rollout.zig`

Add the local alias `const writeJsonString = zts.json_utils.writeJsonString;`, delete the local `fn writeJsonString` at `863-876`. Do not change the eight call sites' text — the alias makes them resolve to the canonical implementation without edits.

**Verify**: `zig build test-rollout` -> success. Existing tests `system_rollout.zig:936, 1015, 1100, 1166` must still pass unmodified.

### Step 3: Add a regression test proving the control-character escape now applies

Add one test per fixed file asserting that a string containing a raw control byte (e.g. `0x01` or `0x1f`, not `\n`/`\r`/`\t`) round-trips through the fixed function as ``/`` rather than being written raw. Model the assertion style after `handler_contract.zig:819-824` `test "writeJsonString escapes correctly"`.

**Verify**: `zig build test-zts test-rollout` -> success; both new tests fail against the pre-fix local implementations (temporarily restore the old local functions locally to confirm, then reapply the fix) — do not skip this red-proof.

## Test plan

- New test in `property_diagnostics.zig`: a diagnostic message containing a control byte escapes correctly via `writeJsonStringContent`.
- New test in `system_rollout.zig`: a rollout plan field (e.g. a handler name or note) containing a control byte escapes correctly via `writeJsonString`.
- Regression: both new tests must fail on the pre-fix local implementations and pass after switching to the canonical one.
- Final: `bash scripts/verify.sh` -> exit 0.

## Done criteria

All must hold:

- [ ] `property_diagnostics.zig` and `system_rollout.zig` both call the canonical `json_utils` escaping functions; neither has its own reimplementation.
- [ ] Both files correctly escape C0 control characters outside `\n`/`\r`/`\t`.
- [ ] `zig build test-zts test-rollout` exit 0.
- [ ] `bash scripts/verify.sh` exit 0.
- [ ] New tests in both files prove the fix, red-proven against the pre-fix code.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match the live code (re-check line numbers first; call-site line numbers especially are prone to drift).
- `property_diagnostics.zig` importing `json_utils.zig` directly creates a circular import (it should not — `json_utils.zig` only imports `std` — but confirm before editing).
- A step's verification fails twice after reasonable local correction.
- Any existing test asserts on the *absence* of control-character escaping in either file's output (i.e. a test that would need its expected output changed) — if so, stop and report rather than silently updating a test's golden output, since that could indicate the raw-byte behavior was intentional somewhere.

## Maintenance notes

- If a third local copy of this pattern surfaces later, prefer grepping for `for (s) |c|` + `'"' => try writer.writeAll("\\\"")`-style switches repo-wide before adding a fourth divergent implementation.
- Reviewers should specifically check the escaped output for a code point in each of the three C0 sub-ranges in `json_utils.zig`'s switch (`0x00...0x08`, `0x0b...0x0c`, `0x0e...0x1f`) to be confident about coverage, not just one sample byte.
