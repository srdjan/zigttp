# zigttp-server Code Analysis TODOs

> Generated from code review findings on 2026-01-19.

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
- [x] **GC scans non-GC pointer payloads stored in JSValue** — `zts/gc.zig:991-1024`, `zts/object.zig:1482-1665`, `zts/builtins.zig:3515-3533`.
  - Root cause: `JSValue.fromPtr` was used for NativeFunctionData/BytecodeFunctionData/ClosureData/GeneratorData/SymbolBox, which are not GC-managed.
  - Fix: introduced external-pointer tagging (`JSValue.fromExternPtr`) and updated call sites so GC ignores non-managed payloads.
  - Impact: prevents GC from treating internal payloads as heap objects.

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
- [ ] **Closure data freed as BytecodeFunctionData** — `zts/object.zig:1415-1450`, `zts/object.zig:1579-1616`.
  - Root cause: closures set `FUNC_IS_BYTECODE`, but `destroyFull` assumes `FUNC_DATA` is `BytecodeFunctionData`.
  - Fix: add an explicit closure tag/slot or discriminator; update `destroyFull` to free `ClosureData` separately.
  - Impact: invalid frees or crashes during teardown when closures are destroyed.
- [ ] **Chunked transfer encoding not handled** — `src/server.zig:304-333`, `src/server.zig:900-936`, `src/server.zig:1551-1559`.
  - Root cause: parser only honors `Content-Length` and ignores `Transfer-Encoding: chunked`.
  - Fix: implement chunked decoding or reject chunked requests with 400/501; ensure keep-alive doesn’t desync.
  - Impact: request smuggling/desync under proxies or clients using chunked encoding.
- [ ] **Bytecode cache is trusted input** — `zts/bytecode_cache.zig:244-296`, `src/zruntime.zig:507-520`.
  - Root cause: deserialized bytecode is executed without validation.
  - Fix: validate opcodes, operand bounds, constant indices, stack effects.
- [ ] **Static file path traversal via symlinks** — `src/server.zig:1099-1115`, `src/server.zig:1491-1511`.
  - Root cause: string-level checks only; symlink inside static dir can escape.
  - Fix: open via `openat`/no-follow or verify realpath prefix.

## Medium
- [ ] **Bytecode constants leak (strings/float boxes/nested functions)** — `zts/object.zig:1421-1450`, `zts/bytecode_cache.zig:172-230`.
  - Root cause: `destroyFull` frees `FunctionBytecode` arrays but never walks `constants` to free heap payloads.
  - Fix: add recursive constant cleanup (strings, float boxes, nested `FunctionBytecode`) or ref-counted constant pools.
  - Impact: memory growth in long-lived runtimes.
- [ ] **NaN treated as non-number in arithmetic** — `zts/value.zig:147-155`, `zts/interpreter.zig:2602-2638`.
  - Root cause: `JSValue.toNumber()` returns null for `nan_val`; arithmetic paths treat null as TypeError.
  - Fix: return `std.math.nan(f64)` for `nan_val` or treat it as a float number.
  - Impact: JS spec violations (`NaN + 1` throws instead of returning NaN).
- [ ] **ARM64 push/pop use frame pointer instead of stack pointer** — `zts/jit/arm64.zig:657-666`.
  - Root cause: `pushReg`/`popReg` use `x29` as base instead of `sp`.
  - Fix: use `.sp` as base register for pre/post-index ops.
  - Impact: corrupts frame pointer if these helpers are used.
- [ ] **Fixed 8KB header storage in buffered parse** — `src/server.zig:954-967`.
  - Root cause: `storage_size` is hard-coded to 8192 while `max_header_bytes` allows larger headers.
  - Fix: size storage based on header length or grow dynamically.
  - Impact: valid large headers fail with `HeaderStorageExhausted`.
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
- [ ] **JIT jump patching lacks range checks** — `zts/jit/baseline.zig:3979-4015`, `zts/jit/arm64.zig:610-628`, `zts/jit/x86.zig:607-661`.
  - Root cause: assumes rel offsets fit imm19/imm26 (AArch64) or rel32 (x86_64).
  - Fix: validate ranges and emit long-branch trampolines when out of range.
  - Impact: incorrect control flow for large functions.

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

## Post-OOP Removal Cleanup (Reassess 2026-01-21)
### Dead / Unreachable Code
- [x] **Remove unreachable class parsing paths** — `zts/parser/parse.zig:1134-1254`, `zts/parser/parse.zig:2029-2137`.
  - Root cause: `kw_class` is rejected in statement and prefix parsing (`zts/parser/parse.zig:165-205`, `zts/parser/parse.zig:1394-1397`), so these functions are never called.
  - Estimated LOC: ~230.
  - Risk: safe if class syntax remains permanently rejected.
  - Dependencies: `zts/parser/ir.zig` class nodes and `zts/parser/scope.zig` class scope/binding; parser tests `zts/parser/parse.zig:3406+`.
- [x] **Remove unreachable new/this/super parsing paths** — `zts/parser/parse.zig:1901-2005`.
  - Root cause: `kw_new`, `kw_this`, `kw_super` are rejected in prefix parsing (`zts/parser/parse.zig:1378-1388`).
  - Estimated LOC: ~100.
  - Risk: safe if `new`/`this`/`super` remain rejected.
  - Dependencies: `NodeTag.new_expr`/`NodeTag.this_expr` in `zts/parser/ir.zig`.
- [x] **Remove unreachable function-expression parsing** — `zts/parser/parse.zig:2008-2026`.
  - Root cause: function expressions are rejected in prefix parsing (`zts/parser/parse.zig:1390-1392`).
  - Estimated LOC: ~20.
  - Risk: safe if function expressions remain unsupported.
  - Dependencies: none beyond parse tests.
- [x] **Remove dead IR nodes for removed constructs** — `zts/parser/ir.zig:80-82`, `zts/parser/ir.zig:115`, `zts/parser/ir.zig:148-149`, `zts/parser/ir.zig:172-176`, `zts/parser/ir.zig:325-328`, `zts/parser/ir.zig:499-516`.
  - Root cause: parser rejects `instanceof`, regex literals, `this`, `new`, and class syntax before IR is built.
  - Estimated LOC: ~70.
  - Risk: safe if those constructs remain unsupported.
  - Dependencies: parser helpers, tests, and any IR tooling.
- [x] **Remove `instanceof` opcode and handler** — `zts/bytecode.zig:138-139`, `zts/interpreter.zig:547-550`, `zts/interpreter.zig:1985-2024`, `zts/parser/codegen.zig:716-737`, `zts/parser/ir.zig:80`.
  - Root cause: parser rejects `instanceof` (`zts/parser/parse.zig:1489-1492`), so bytecode is never emitted.
  - Estimated LOC: ~60.
  - Risk: safe if bytecode cache is invalidated or version-bumped; otherwise needs compatibility handling for old bytecode.
  - Dependencies: bytecode cache, any precompiled bytecode.
- [x] **Remove delete opcodes and stack effect entries** — `zts/bytecode.zig:122-123`, `zts/interpreter.zig:547`, `zts/interpreter.zig:562`.
  - Root cause: `delete` is rejected in parser (`zts/parser/parse.zig:1406-1408`) and not code-generated.
  - Estimated LOC: ~10.
  - Risk: safe if bytecode cache invalidated.
  - Dependencies: bytecode cache validation/compat.
- [ ] **Remove regex literal artifacts** — `zts/parser/token.zig:17`, `zts/parser/tokenizer.zig:27-45`, `zts/parser/tokenizer.zig:185-188`, `zts/parser/ir.zig:115`, `zts/parser/ir.zig:325-328`, `zts/parser/error.zig:18`.
  - Partial: IR regex literal node removed; tokenizer-level regex handling remains for error reporting.
- [x] **Bump bytecode cache version after opcode removals** — `zts/bytecode_cache.zig:304`.
  - Notes: invalidates cached bytecode produced with removed `instanceof`/`delete` opcodes.
  - Root cause: regex literals are disabled at the tokenizer and rejected at parse time.
  - Estimated LOC: ~30–40.
  - Risk: safe if regex is permanently removed; verify JSX lookahead no longer needs `can_be_regex`.
  - Dependencies: tokenizer tests and any JSX edge-case handling that toggles `can_be_regex`.
- [x] **Remove dead array mutator fast-path ids** — `zts/object.zig:636-637`, `zts/interpreter.zig:2922`.
  - Root cause: mutating array methods are removed from builtins (`zts/builtins.zig:2910-2930`), so these fast paths are never reached.
  - Estimated LOC: ~5–8.
  - Risk: safe.
  - Dependencies: builtin dispatch only.

### Redundant / Duplicate Logic
- [ ] **Consolidate duplicate "unsupported feature" error checks** — `zts/parser/parse.zig:1595-1611`, `zts/parser/parse.zig:1883-1890`, plus class errors in both statement/prefix parsing (`zts/parser/parse.zig:165-205`, `zts/parser/parse.zig:1394-1397`).
  - Root cause: multiple hard-coded checks for features already removed from builtins or tokenizer.
  - Estimated LOC: ~20–30.
  - Risk: low; keep error quality while reducing duplication.
  - Dependencies: parser tests expecting specific error strings.

### Needs Careful Review (Big Behavior Changes)
- [ ] **Prototype system remnants (defer: prototypes stay as placeholders)** — `zts/object.zig:1291-1813`, `zts/context.zig:186-192`, `zts/context.zig:660-667`, `zts/builtins.zig:2735-2997`, `zts/interpreter.zig:2035-2890`.
  - Note: Keep as-is for now per product direction; only revisit when prototype removal is back on the roadmap.
  - Dependencies: builtins initialization, call semantics (`call_method`), property lookup paths.
- [ ] **`this` binding remnants (review only if `this` removal resumes)** — `zts/context.zig:144-151`, `zts/context.zig:1386-1395`, `zts/context.zig:1483-1486`, `zts/parser/codegen.zig:849-878`, `zts/interpreter.zig:2839-2890`.
  - Note: Keep for now because method dispatch still relies on implicit receiver.
- [ ] **Async/Promise remnants (defer: async/Promise placeholders stay)** — `zts/bytecode.zig:141-143`, `zts/interpreter.zig:1836-1864`, `zts/interpreter.zig:2991-3023`.
  - Note: Keep as-is for now per product direction; only revisit if async is fully removed.
- [ ] **Prototype atom tests after removal (defer)** — `zts/bytecode_cache.zig:1414-1429`, `zts/object.zig:2320-2340`.
  - Note: Keep tests while prototypes remain.
