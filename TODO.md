# zigttp-server Code Analysis TODOs

> Generated from code review findings on 2026-01-17.

## Critical
- [x] **GC major sweep frees live objects** — `zts/gc.zig:815-907`.
  - Root cause: mark bits are never set; `TenuredHeap.setMark` is unused.
  - Fix: mark-on-visit, keep a ptr->index map (or header mark) and only push newly marked objects.
- [x] **GC minor collection drops live nursery objects** — `zts/gc.zig:615-667`.
  - Root cause: only roots/remembered-set entries are evacuated; no transitive closure scan.
  - Fix: implement Cheney-style evacuation queue that scans children and updates pointers.
- [x] **ARM64 JIT pointer / MemTag checks incorrect** — `zts/jit/baseline.zig:2102-2115, 2124-2126, 2336-2356`.
  - Root cause: masking with `2` / `3` instead of `0x7` / `0xF`.
  - Fix: use `0x7` for pointer tags and `0xF` for MemTag extraction.

## High
- [x] **JIT stack push lacks bounds checks** — `zts/jit/baseline.zig:1549-1597, 2213-2217`.
  - Root cause: compiled pushes don’t enforce `ctx.stack` capacity.
  - Fix: add stack limit checks and deopt/throw on overflow.
- [x] **Static file cache UAF** — `src/server.zig:1307-1317`.
  - Root cause: `remove()` frees `entries` key before removing LRU node that uses same key bytes.
  - Fix: remove LRU entry first, or store independent key allocations.
- [x] **Static cache not thread-safe** — `src/server.zig:1099-1416`.
  - Root cause: shared maps/LRU mutated without synchronization.
  - Fix: guard with mutex or isolate cache per thread.
- [x] **Threaded backend reads only once** — `src/server.zig:252-265` + `parseRequestFromBuffer`.
  - Root cause: single 8KB read can truncate headers/body; keep-alive can desync.
  - Fix: incremental read until `\r\n\r\n`, then read `Content-Length` bytes.
- [ ] **Bytecode cache is trusted input** — `zts/bytecode_cache.zig:244-296`, `src/zruntime.zig:507-520`.
  - Root cause: deserialized bytecode is executed without validation.
  - Fix: validate opcodes, operand bounds, constant indices, stack effects.
- [ ] **Static file path traversal via symlinks** — `src/server.zig:1099-1115`, `src/server.zig:1491-1511`.
  - Root cause: string-level checks only; symlink inside static dir can escape.
  - Fix: open via `openat`/no-follow or verify realpath prefix.

## Medium
- [ ] **Thread-local runtime cache leak on thread exit** — `zts/pool.zig:205-215`, `zts/pool.zig:347-355`.
  - Root cause: TLS runtime not reclaimed unless thread calls `releaseThreadLocal()`.
  - Fix: worker shutdown hook or pool-managed TLS registry.
- [ ] **TypeScript stripper rejects enums/namespaces/decorators** — `zts/stripper.zig:989-1013`.
  - Root cause: hard error on TS features; limits compatibility.
  - Fix: implement stripping or fallback with clear error.

## Low
- [x] **writeBarrier uses wrong pointer type** — `zts/gc.zig:977-980`.
  - Root cause: `toPtr(*anyopaque)` yields `**anyopaque`.
  - Fix: use `toPtr(u8)` or `toPtr(anyopaque)`.

## Performance / Architectural Follow-ups
- [ ] **Request object creation causes hidden-class churn** — `src/zruntime.zig:617-640`.
  - Use prebuilt shapes + slot writes for `method`, `url`, `headers`.
- [ ] **String concatenation hot path allocs** — `zts/interpreter.zig:2595-2688`.
  - Consider rope/concat builder or `concatMany` for multi-term expressions.
- [ ] **Bytecode cache string deserialization double-copies** — `zts/bytecode_cache.zig:210-223`.
  - Add “internOwned” to adopt buffer.
- [ ] **Static file caching alloc spikes** — `src/server.zig:1154-1185`.
  - Use mmap or streaming for large files; stricter cache thresholds.
- [ ] **PIC cache size** — `zts/interpreter.zig:326-356`.
  - Consider adaptive sizing per function for large scripts.

## Missing Tests
- [ ] GC transitive closure + remembered-set correctness (minor/major).
- [ ] ARM64 JIT tag checks + call fast paths.
- [ ] JIT stack overflow behavior.
- [ ] HTTP partial reads, long headers, keep-alive pipelining.
- [ ] Static cache concurrency + LRU correctness.
- [ ] Corrupted bytecode cache rejection.
