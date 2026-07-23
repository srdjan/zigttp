# Plan 010: Stop re-scanning consumed chunked-body bytes on every socket read

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only a4a731bd -- packages/runtime/src/server.zig packages/runtime/src/http_parser.zig`
> Empty means no drift. If any path appears, re-open it and compare against the Current state excerpts before editing.

## Status

- **Priority**: P1
- **Effort**: M
- **Risk**: LOW
- **Depends on**: none
- **Category**: perf
- **Planned at**: commit `a4a731bd` and 2026-07-02

## Why this matters

`ConnectionPool.readRequestData` re-parses the entire accumulated request buffer from scratch after every `posix.read`. For `Content-Length` bodies this is cheap (`findHeaderEnd` returns as soon as it sees `\r\n\r\n`, near the front of the buffer). For chunked bodies it is not: `chunkedBodyConsumed` walks every chunk from `pos = 0` on every call, with no state carried between reads. With a 16KB read buffer, an n-byte chunked body delivered over many small reads costs O(n²/16KB) CPU instead of O(n). Chunk boundaries and read timing are client-controlled, so this is a real CPU-amplification vector reachable by any client sending a chunked request, not just a latent inefficiency.

## Current state

- `packages/runtime/src/server.zig:820-860` — `ConnectionPool.readRequestData`. The read loop (`843-859`) appends new bytes to `data: std.ArrayList(u8)` then unconditionally calls `self.completeRequestLength(data.items)` (`856`) on the whole buffer.
- `packages/runtime/src/server.zig:874-899` — `ConnectionPool.completeRequestLength`. For `transfer_encoding == .chunked` (`887-897`), it calls `http_parser.chunkedBodyConsumed(data[body_start..], self.server.config.max_body_size)` with the full body-so-far slice, every time.
- `packages/runtime/src/http_parser.zig:448-483` — `pub fn chunkedBodyConsumed(body: []const u8, max_body_size: usize) !?usize`. Starts `pos: usize = 0; decoded_len: usize = 0;` and walks chunk-size lines from the beginning on every call. Returns `null` (caller should read more) or the number of encoded bytes consumed once the terminating chunk is found.
- Existing tests already exercise this function across multiple reads: `packages/runtime/src/server.zig:2787` `test "threaded readRequestData handles chunked body across reads"` and `packages/runtime/src/http_parser.zig` has no direct multi-call test for `chunkedBodyConsumed` itself (search `chunkedBodyConsumed` in that file's `test` blocks to confirm before writing new ones — do not duplicate).
- `chunkedBodyConsumed` is also called for a single complete buffer from `packages/runtime/src/server.zig:3046` `test "parseRequestFromBuffer decodes chunked transfer encoding"` (single-call path, not the incremental one this plan targets) — do not change that call site's behavior for a single already-complete buffer.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Build | `zig build` | exit 0 |
| Server/runtime facade tests | `zig build test-server` | success |
| ZRuntime tests | `zig build test-zruntime` | success |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Full local gate | `bash scripts/verify.sh` | exit 0 |

## Scope

**In scope, the only files/directories to modify:**

- `packages/runtime/src/http_parser.zig` — give `chunkedBodyConsumed` (or a new sibling entry point) a way to resume from a prior offset instead of restarting at 0.
- `packages/runtime/src/server.zig` — thread the resume state through `readRequestData`'s loop and whatever holds per-connection parse state (e.g. `PendingRequestBytes`, or a new small struct local to `readRequestData`).

**Out of scope:**

- The `Content-Length` path in `completeRequestLength` (already O(1) per call; do not touch `parseContentLength` or `findHeaderEnd`).
- Any change to chunked-encoding wire semantics, trailer handling, or the `MAX_CHUNK_SIZE_LINE_BYTES` / `MAX_CHUNK_TRAILER_BYTES` limits.
- `decodeChunkedBody` (the separate full-buffer decode-to-owned-slice function below `chunkedBodyConsumed` in the same file) — only the incremental *length-detection* path is in scope.
- `parseRequestFromBuffer`'s single-call use of `chunkedBodyConsumed` (already receives a complete buffer; must keep working unchanged).

## Git/workflow guidance

- Branch: work on `main`.
- Commit style: Conventional Commits, e.g. `fix(runtime): avoid re-scanning consumed chunked-body bytes on every read`.
- Do not push or open a PR unless the operator asks.

## Steps

### Step 1: Add resumable parse state to `chunkedBodyConsumed`

Change the function to accept (or thread through a small caller-owned struct) the last-validated byte offset and running decoded length from the previous call, so it only scans bytes appended since then. Keep the return contract identical (`!?usize`: `null` = need more bytes, `usize` = total encoded bytes consumed including terminator/trailers). The trailer-scanning inner loop (`458-470`) already tracks `trailer_start` relative to the current call's `pos`; make sure resumed calls that land mid-trailer still work (i.e., the resume state must distinguish "mid chunk-size-line", "mid chunk data", and "mid trailer").

**Verify**: `zig build test-zts` is not needed here (this file is package `runtime`, not `zts`) — just confirm it still compiles: `zig build` -> exit 0.

### Step 2: Thread the resume state through `readRequestData`

In `server.zig:820-860`, replace the unconditional `self.completeRequestLength(data.items)` re-scan with a call that passes and updates the resume state introduced in Step 1. The state should live for the duration of one `readRequestData` call (per-request, not per-connection — a new request starts a fresh chunked parse).

**Verify**: `zig build test-server` -> success. In particular `test "threaded readRequestData handles chunked body across reads"` (`server.zig:2787`) must still pass unmodified in its assertions (only the internal parse cost should change, not observable behavior).

### Step 3: Add a regression test that proves the quadratic behavior is gone

Add a test that feeds a chunked body across many small reads (e.g. 50+ reads of a few hundred bytes each, several KB total) and asserts either (a) a direct call-count/byte-scanned counter shows linear total work, or (b) if a cost counter isn't practical, a wall-clock relative comparison against a naive re-scan baseline is too flaky for CI — prefer (a). If neither is practical without invasive instrumentation, at minimum add a test with enough reads/chunks that a reintroduced O(n²) rescan would be the obviously-wrong implementation on inspection, and note in the test's doc comment that it is a functional regression test, not a perf regression test.

**Verify**: `zig build test-server test-zruntime` -> success; the new test fails against the pre-fix implementation (temporarily revert Step 1-2 locally to confirm, then reapply) — do not skip this red-proof.

## Test plan

- New test(s) in `packages/runtime/src/server.zig` (co-located with the existing `test "threaded readRequestData handles chunked body across reads"`), modeled after that test's structure (uses the same threaded read-loop harness).
- Regression: verify the new test fails on the pre-fix code (Step 3's red-proof) and passes after.
- Edge cases to cover: a chunk-size line split across a read boundary, chunk data split across a read boundary, and the trailer split across a read boundary — each should resume correctly rather than re-validating already-seen bytes.
- Final: `bash scripts/verify.sh` -> exit 0.

## Done criteria

All must hold:

- [ ] `chunkedBodyConsumed` (or its replacement) no longer rescans previously-validated bytes from `pos = 0` on repeated calls with growing input.
- [ ] `zig build test-server test-zruntime` exit 0.
- [ ] `bash scripts/verify.sh` exit 0.
- [ ] New test(s) cover chunk-size-line, chunk-data, and trailer split-across-reads cases, and are proven to fail on the pre-fix implementation.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match the live code (re-check line numbers first; the file may have shifted since `a4a731bd`).
- The resume-state design can't cleanly represent "mid-trailer" without effectively duplicating the trailer-scan loop's own state machine — if so, report the shape of the problem rather than shipping a partial fix.
- A step's verification fails twice after reasonable local correction.
- The fix requires touching `decodeChunkedBody` or trailer/limit semantics (out of scope).

## Maintenance notes

- If a future change adds another incremental-parse path (e.g. streaming request bodies to the handler), it should reuse this resume-state pattern rather than reintroducing a full-rescan loop.
- Reviewers should specifically check that the trailer-boundary and chunk-size-line-boundary split cases are exercised, since those are the parts of `chunkedBodyConsumed` most likely to have subtle resume-state bugs.
