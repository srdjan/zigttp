# TypeScript-to-Zig Transpilation Plan

## Deliverable

Create `docs/ts-to-zig-transpiler.md` with the full plan below, then implement it checkpoint by checkpoint.

---

## Overview

Replace the current pattern-matching AOT approach with a general-purpose IR-to-Zig transpiler. The existing `HandlerAnalyzer` recognizes only static URL routing patterns. The transpiler will convert arbitrary TypeScript handler IR into Zig source code that the Zig compiler optimizes through its full pipeline (AST -> ZIR -> AIR -> LLVM -> machine code).

### Pipeline

```
handler.ts -> stripper.zig (strip types) -> JsParser (parse to IR) -> IrTranspiler (IR -> Zig source) -> Zig compiler -> native binary
```

The transpiler runs at build time inside `tools/precompile.zig`. It reads the same IR (`IrView`) the current `HandlerAnalyzer` reads, but instead of pattern-matching for static responses, it walks the full IR tree and emits equivalent Zig source code. The generated Zig file replaces the current `embedded_handler.zig` template.

### Design Constraints

1. **Types are stripped before IR.** `stripper.zig` removes all TypeScript annotations. The IR is untyped. The transpiler must work with dynamic JS semantics or infer types from usage.
2. **zts restricts JS features.** 46 JS features (while, try/catch, class, var, ==, ++, etc.) and 7 TS features (enum, namespace, etc.) are rejected at parse time. The transpiler only needs to handle the supported subset.
3. **Fallback is mandatory.** Any IR node the transpiler cannot handle emits `error.AotBail`, falling through to the bytecode interpreter. The transpiler never needs to be 100% complete.
4. **Build-time only.** The transpiler runs during `zig build`, generating a `.zig` file that becomes part of the binary. Zero runtime overhead for the transpilation itself.

### Relation to Existing AOT

The current pattern-matching AOT (`HandlerAnalyzer` + `precompile.zig` code generation) becomes Checkpoint 0 - it already works. The transpiler subsumes it: once the transpiler handles if-chains with string comparisons and Response.* calls, the pattern matcher is redundant. During development, both coexist - the transpiler is tried first, falling back to pattern matching, falling back to interpreter.

---

## IR Node Coverage

The IR has 95 node types (defined in `zts/parser/ir.zig`). The transpiler handles them in priority order based on what appears in real handlers.

### Tier 1: Core (Checkpoints 1-3)
Nodes that appear in every handler.

| IR Node | Zig Output | Notes |
|---|---|---|
| `lit_int` | `@as(i32, 42)` | NaN-boxed at boundaries |
| `lit_float` | `@as(f64, 3.14)` | |
| `lit_string` | `"hello"` | Zig string literal |
| `lit_bool` | `true` / `false` | |
| `lit_null` | `JSValue.null_val` | |
| `lit_undefined` | `JSValue.undefined_val` | |
| `identifier` | `local_N` or `arg_N` | Binding slot -> Zig local name |
| `binary_op` (arithmetic) | `a + b`, `a * b`, etc. | Integer fast path, float fallback |
| `binary_op` (comparison) | `a == b`, `a < b`, etc. | `std.mem.eql` for strings |
| `binary_op` (logical) | `a and b`, `a or b` | Short-circuit via Zig `if` |
| `unary_op` | `-a`, `!a` | |
| `member_access` | Shape-aware slot read | `req_obj.getSlot(slot)` or property lookup |
| `call` | Direct function call | `aot_funcname(ctx, args)` |
| `method_call` | Method dispatch | `Response.json(...)` -> `http.createResponse(...)` |
| `return_stmt` | `return val;` | |
| `var_decl` | `const local_N = expr;` | `let` -> `var`, `const` -> `const` |
| `if_stmt` | `if (cond) { ... } else { ... }` | |
| `block` | `{ ... }` | Scope via Zig blocks |
| `expr_stmt` | `_ = expr;` | Discard result |
| `object_literal` | Emit JSON at comptime or build at runtime | Static -> string literal, dynamic -> buffer build |
| `object_property` | Key-value in object | |
| `array_literal` | `[_]JSValue{ ... }` | |

### Tier 2: Functions and Control Flow (Checkpoints 4-5)
Needed for handlers with helper functions and iteration.

| IR Node | Zig Output |
|---|---|
| `function_decl` | `fn aot_name(ctx, args) !JSValue { ... }` |
| `function_expr` | Anonymous function -> named Zig fn |
| `arrow_function` | Same as function_expr |
| `for_of_stmt` | `for (array.items) \|item\| { ... }` or `while` loop |
| `ternary` | `if (cond) then_val else else_val` |
| `assignment` | `local_N = expr;` |
| `template_literal` | `std.fmt.bufPrint` or concatenation |
| `spread` | Inline expansion |
| `computed_access` | `obj.getProperty(key)` |

### Tier 3: Advanced (Checkpoint 6+)
Edge cases and less common patterns.

| IR Node | Zig Output |
|---|---|
| `switch_stmt` | Zig `switch` on tagged union or if-chain |
| `for_stmt` | `while (cond) : (update) { ... }` |
| `sequence_expr` | Sequential evaluation, return last |
| `optional_chain` | `if (obj) \|o\| o.prop else null` |
| `destructuring` (array_pattern, object_pattern) | Multiple `const` declarations |

### Not Transpiled (Always fall back to interpreter)
These nodes emit `error.AotBail`:

- `jsx_element`, `jsx_fragment`, `jsx_text`, `jsx_expr_container`, `jsx_attribute`, `jsx_spread_attribute` - JSX requires the full runtime `h()` + `renderToString()` pipeline
- `import_decl`, `export_decl` and variants - Module system
- `await_expr`, `yield_expr` - Async/generators (stubs only in zts)
- `try_stmt`, `throw_stmt` - Exception handling (unsupported in zts, parser rejects)
- `while_stmt`, `do_while_stmt` - Parser rejects these
- `labeled_stmt`, `debugger_stmt` - Debug/legacy
- `for_in_stmt` - Parser rejects

---

## Type Strategy

TypeScript types are stripped before parsing by `stripper.zig`. This includes all annotations: `string`, `number`, `any`, interfaces, generics, union types, type aliases - everything. The IR is fully untyped. Notably, `any` is not a restricted feature - it is valid TypeScript that gets silently removed. This means the transpiler cannot distinguish between a variable originally typed as `string`, typed as `any`, or written in plain untyped JavaScript. All three produce identical IR.

The transpiler must therefore infer types entirely from runtime usage patterns in the IR. Three strategies handle this, applied in order of preference:

### 1. Static Type Inference (preferred - generates unboxed Zig)
Walk the IR to infer types from usage patterns:
- `lit_int` -> `i32`
- `lit_float` -> `f64`
- `lit_string` -> `[]const u8`
- `lit_bool` -> `bool`
- Binary arithmetic on two `i32` operands -> `i32` result
- String concatenation -> `[]const u8`
- `Response.json(...)` -> `JSValue` (response object)
- Function return type inferred from return statements

When all paths through a function use the same types, emit unboxed Zig code:

```zig
// function fibonacci(n) { if (n <= 1) return n; ... }
// Inferred: n: i32, return: i32
fn aot_fibonacci(n: i32) i32 {
    if (n <= 1) return n;
    // ...
}
```

### 2. Type Guards at Boundaries (when inference succeeds for the fast path but input types are unknown)
When a function's body uses integer-only operations but parameter types cannot be proven (the original TypeScript may have used `any`, or it may have been untyped JS), emit a typed fast path with a runtime guard:

```zig
fn aot_fibonacci_entry(ctx: *Context, args: []const JSValue) !JSValue {
    // Type guard: verify argument is integer
    if (args[0].isInt()) {
        return JSValue.fromInt(aot_fibonacci(args[0].getInt()));
    }
    return error.AotBail; // Fall back to interpreter
}
```

### 3. JSValue Fallback (when types cannot be inferred at all)
When operations mix types, use dynamic values, or the inference cannot determine a single type (common with `any`-originated values), operate on `JSValue` directly using helper functions:

```zig
const result = try helpers.jsAdd(ctx, a, b); // Handles int+int, float+float, string concat
```

### Type Mapping Table

| JS Runtime Type | Zig Type (unboxed) | Zig Type (boxed) |
|---|---|---|
| integer | `i32` | `JSValue` (NaN-boxed int) |
| float | `f64` | `JSValue` (NaN-boxed float) |
| string | `[]const u8` | `JSValue` (pointer to JSString) |
| boolean | `bool` | `JSValue` (.true_val/.false_val) |
| null | - | `JSValue.null_val` |
| undefined | - | `JSValue.undefined_val` |
| object | - | `JSValue` (pointer to JSObject) |
| array | `[]JSValue` | `JSValue` (pointer to JSObject) |

---

## Generated Code Architecture

### File Structure

The transpiler generates a single Zig file (`src/generated/embedded_handler.zig`) containing:

```zig
//! Auto-generated from handler.tsx
//! Transpiled with: zig build -Dhandler=handler.tsx -Daot=true

const std = @import("std");
const zq = @import("zts");

pub const has_aot = true;
pub const aot_metadata = .{ ... };

// --- Helper functions (inlined) ---

inline fn jsStrictEqStr(val: zq.JSValue, comptime expected: []const u8) bool { ... }
inline fn jsonEscapeInto(buf: []u8, src: []const u8) usize { ... }

// --- Transpiled helper functions ---

fn aot_fibonacci(n: i32) i32 {
    if (n <= 1) return n;
    var a: i32 = 0;
    var b: i32 = 1;
    var i: i32 = 2;
    while (i <= n) : (i += 1) {
        const temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

fn aot_processData(ctx: *zq.Context, name: []const u8) !zq.JSValue {
    // builds {"message":"Hello, <name>","timestamp":<Date.now()>}
    // Date.now() cannot be transpiled -> AotBail for this function
    return error.AotBail;
}

// --- Main handler entry point ---

pub fn aotHandler(ctx: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue {
    if (args.len < 1) return error.AotBail;
    const req_val = args[0];
    if (!req_val.isObject()) return error.AotBail;
    const req_obj = req_val.toPtr(zq.JSObject);

    // Extract request fields (shape-aware)
    const url = extractRequestField(ctx, req_obj, .url) orelse return error.AotBail;
    const method = extractRequestField(ctx, req_obj, .method) orelse return error.AotBail;

    // Transpiled if-chain from handler body
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, url, "/")) {
        // JSX route - cannot transpile, bail
        return error.AotBail;
    }
    if (std.mem.eql(u8, url, "/api/health")) {
        return zq.http.createResponse(ctx, "{\"status\":\"ok\",\"runtime\":\"zts\"}", 200, "application/json");
    }
    if (std.mem.eql(u8, url, "/api/compute")) {
        const result = aot_fibonacci(30);
        // Build JSON response with runtime value
        var buf: [256]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "{{\"computation\":\"fibonacci\",\"n\":30,\"result\":{d}}}", .{result}) catch return error.AotBail;
        return zq.http.createResponse(ctx, buf[0..len], 200, "application/json");
    }
    // ... more routes ...
    return error.AotBail;
}

pub const bytecode = [_]u8{ ... }; // Interpreter fallback
```

### Request Field Extraction

Shared helper for shape-aware property access (used by all routes):

```zig
fn extractRequestField(ctx: *zq.Context, obj: *zq.JSObject, comptime field: zq.Atom) ?[]const u8 {
    const pool = ctx.hidden_class_pool orelse return null;
    if (ctx.http_shapes) |shapes| {
        if (obj.hidden_class_idx == shapes.request.class_idx) {
            const val = switch (field) {
                .url => obj.getSlot(shapes.request.url_slot),
                .method => obj.getSlot(shapes.request.method_slot),
                else => return null,
            };
            if (val.isString()) return val.toPtr(zq.JSString).data();
        }
    }
    if (obj.getOwnProperty(pool, field)) |val| {
        if (val.isString()) return val.toPtr(zq.JSString).data();
    }
    return null;
}
```

---

## Checkpoints

Each checkpoint is independently testable. A checkpoint is complete when all its tests pass and the build succeeds with `zig build -Dhandler=examples/handler.jsx -Daot=true`.

### Checkpoint 0: Current State (DONE)
- [x] Pattern-matching AOT for static URL dispatch
- [x] Build system integration (`-Daot=true`)
- [x] Runtime dispatch with `error.AotBail` fallback
- [x] Stub for non-AOT builds

**Test:** `zig build -Dhandler=examples/handler.jsx -Daot=true && zig build test`

### Checkpoint 1: IR Transpiler Scaffold
**Goal:** Create the transpiler module that walks IR and emits Zig source. Initially handles only literals and simple expressions, bailing on everything else.

**Files:**
- New: `tools/transpiler.zig` - IR-to-Zig source emitter
- Modified: `tools/precompile.zig` - Invoke transpiler instead of pattern matcher when `--aot` is passed

**Implementation:**
1. Create `IrTranspiler` struct with `IrView` input and `ArrayList(u8)` output buffer
2. Implement `transpileNode(idx: NodeIndex) !void` dispatch on `NodeTag`
3. Handle Tier 1 literals: `lit_int`, `lit_float`, `lit_string`, `lit_bool`, `lit_null`, `lit_undefined`
4. Handle `identifier` via binding slot -> `local_N` / `arg_N` name mapping
5. Handle `binary_op` for arithmetic and comparison (emit infix operators)
6. Handle `unary_op` (emit prefix operators)
7. Handle `var_decl` -> `const local_N = <expr>;` or `var local_N = <expr>;`
8. Handle `return_stmt` -> `return <expr>;`
9. Handle `expr_stmt` -> `_ = <expr>;`
10. Handle `block` -> `{ <stmts> }`
11. Everything else -> `return error.AotBail;`
12. Wire into `precompile.zig`: if transpiler produces output, use it; else fall back to pattern matcher

**Test:**
```bash
# Minimal handler that only returns a literal
echo 'function handler(req) { return Response.json({status: "ok"}); }' > /tmp/test.js
zig build -Dhandler=/tmp/test.js -Daot=true
# Verify generated file has transpiled code
cat src/generated/embedded_handler.zig
```

**Verification:** Build succeeds. Generated file contains transpiled Zig for the simple handler. All existing tests still pass.

### Checkpoint 2: Function Transpilation
**Goal:** Transpile the handler function signature and if-chain routing. Extract request properties.

**Files:**
- Modified: `tools/transpiler.zig` - Function handling, if-chains, member access
- Modified: `tools/precompile.zig` - Handler function discovery (reuse `findHandlerFunction`)

**Implementation:**
1. Handle `function_expr` / `arrow_function`: emit `fn aot_handler(ctx: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue`
2. Handle `if_stmt` -> `if (<cond>) { <then> } else { <else> }`
3. Handle `member_access` on request parameter -> shape-aware field extraction
4. Handle `method_call` on `Response.json/text/html` -> `zq.http.createResponse(...)` with compile-time serialized body
5. Handle `binary_op(.strict_eq)` with string comparison -> `std.mem.eql(u8, ...)`
6. Handle `binary_op(.and_op)` -> short-circuit `and`
7. Handle `call` to `Response.*` static methods
8. Emit shared `extractRequestField` helper

**Test:**
```bash
zig build -Dhandler=examples/handler.jsx -Daot=true
zig build run -Dhandler=examples/handler.jsx -Daot=true -- -p 3000 &
curl localhost:3000/api/health  # Should hit AOT path
curl -X POST localhost:3000/api/health  # Method-filtered route
```

**Verification:** Routes with static responses are transpiled. Dynamic routes (containing `Date.now()`, `request.headers`, etc.) emit `AotBail`. Differential test: responses match interpreter-only mode byte-for-byte on static routes.

### Checkpoint 3: Object and JSON Construction
**Goal:** Transpile object literals with mixed static/dynamic values. Build JSON responses at runtime for objects containing variables.

**Files:**
- Modified: `tools/transpiler.zig` - Object literal transpilation, JSON buffer building
- New: `zts/json_escape.zig` (or inline in transpiler) - JSON string escaping

**Implementation:**
1. Classify object literals: fully static (all literal values) vs. partially dynamic
2. Fully static objects: serialize to JSON string at compile time (reuse existing `serializeObjectLiteral` logic)
3. Partially dynamic objects: emit `std.fmt.bufPrint` or manual buffer construction with JSON escaping
4. Handle string concatenation (`binary_op(.add)` with strings) -> `std.mem.concat` or buffer write
5. Handle `array_literal` -> serialize static arrays, emit runtime construction for dynamic
6. Handle `template_literal` -> buffer concatenation of parts

**Test:**
```bash
# Handler with dynamic JSON response
echo 'function handler(req) {
  const url = req.url;
  return Response.json({path: url, method: req.method});
}' > /tmp/test_dynamic.js
zig build -Dhandler=/tmp/test_dynamic.js -Daot=true
zig build run -Dhandler=/tmp/test_dynamic.js -Daot=true -- -p 3000 &
curl localhost:3000/test  # Should return {"path":"/test","method":"GET"}
```

**Verification:** Dynamic JSON responses produce correct output. JSON special characters in URLs are properly escaped. No buffer overflows on long URLs (test with 4000-char URL).

### Checkpoint 4: Helper Function Compilation
**Goal:** Transpile top-level helper functions to native Zig. Enable the handler to call them directly.

**Files:**
- Modified: `tools/transpiler.zig` - Function discovery, type inference, function compilation
- Modified: `tools/precompile.zig` - Multi-function IR walk

**Implementation:**
1. Walk program-level statements to find all `function_decl` nodes
2. For each function, attempt type inference:
   - Track all operations on parameters
   - If all arithmetic -> infer `i32` parameters and return
   - If string operations -> infer `[]const u8`
   - Mixed or unclear -> use `JSValue`
3. Compile function body:
   - `if_stmt` -> Zig `if`
   - `for_of_stmt` with `range()` -> `while` loop with counter
   - `assignment` -> Zig assignment
   - `binary_op` (all arithmetic) -> native Zig arithmetic
   - Recursive calls -> direct Zig function call
4. For integer-specialized functions, emit unboxed version + type guard wrapper:
   ```zig
   fn aot_fibonacci(n: i32) i32 { ... }
   fn aot_fibonacci_entry(ctx: *Context, args: []const JSValue) !JSValue {
       if (args[0].isInt()) return JSValue.fromInt(aot_fibonacci(args[0].getInt()));
       return error.AotBail;
   }
   ```
5. In handler body, replace `call` to known functions with direct Zig call

**Test:**
```bash
zig build -Dhandler=examples/handler.jsx -Daot=true
zig build run -Dhandler=examples/handler.jsx -Daot=true -- -p 3000 &
curl localhost:3000/api/compute  # fibonacci(30) should return 832040
```

**Verification:** `fibonacci(30)` returns correct result (832040). Integer-specialized path has zero JSValue boxing overhead. Benchmark: compare transpiled fibonacci vs interpreter fibonacci.

### Checkpoint 5: Control Flow Completeness
**Goal:** Handle remaining control flow: for-of loops, ternary expressions, switch statements, destructuring.

**Files:**
- Modified: `tools/transpiler.zig` - Remaining control flow nodes

**Implementation:**
1. `for_of_stmt` on array -> `for (items) |item| { ... }`
2. `for_of_stmt` with `range(start, end)` -> `var i = start; while (i < end) : (i += 1) { ... }`
3. `ternary` -> `if (cond) then_val else else_val`
4. `switch_stmt` -> chain of `if`/`else if` (Zig switch requires comptime-known values)
5. `array_pattern` / `object_pattern` destructuring -> multiple `const` declarations
6. `computed_access` `obj[key]` -> `obj.getProperty(key)`
7. `optional_chain` `obj?.prop` -> `if (obj.isObject()) obj.getSlot(...) else JSValue.undefined_val`
8. `assignment` with existing variable -> `local_N = expr;`
9. `spread` in function calls -> inline argument expansion

**Test:**
```bash
# Handler using for-of and ternary
echo 'function handler(req) {
  const items = [1, 2, 3, 4, 5];
  let sum = 0;
  for (const item of items) { sum = sum + item; }
  const status = sum > 10 ? "high" : "low";
  return Response.json({sum: sum, status: status});
}' > /tmp/test_control.js
zig build -Dhandler=/tmp/test_control.js -Daot=true
```

**Verification:** All control flow constructs produce correct results. Edge cases: empty arrays, single-element arrays, deeply nested ternaries.

### Checkpoint 6: Async Transpilation (Colorblind Pattern)
**Goal:** When zts gains async support, transpile `async/await` to Zig's `Io` parameter pattern. This checkpoint is future work - blocked on zts implementing Promises and an event loop.

**Design (for when async is available):**

The "colorblind" pattern eliminates function coloring. Every function that might do I/O receives an `Io` parameter. Sync callers pass a no-op `Io`, async callers pass the real one.

| TypeScript | Zig (transpiled) |
|---|---|
| `async function fetch(url)` | `fn aot_fetch(io: *Io, url: []const u8) !Response` |
| `const data = await fetch(url)` | `const data = try aot_fetch(io, url);` |
| `Promise.all([a(), b()])` | `const t1 = io.async(aot_a, .{io}); const r2 = try aot_b(io); const r1 = t1.await(io);` |
| `try { await f() } catch (e) { }` | `const result = aot_f(io) catch \|err\| { ... };` |

**Blocked on:** zts `await_val`/`make_async` opcodes are stubs. No Promise implementation. No event loop. This checkpoint activates when those are implemented.

### Checkpoint 7: Full Handler Coverage
**Goal:** Transpile 100% of `examples/handler.jsx` routes, including JSX routes (via bail) and error handling routes.

**Implementation:**
1. Audit every route in `examples/handler.jsx` and `examples/handler.ts`
2. For each route, verify transpiled output matches interpreter output
3. Routes that use untranspilable features (JSX, JSON.tryParse) gracefully bail
4. Measure: what percentage of requests hit the AOT fast path vs. interpreter fallback

**Success metric:** For `examples/handler.jsx`:
- `/api/health` - AOT (static JSON)
- `/api/echo` - AOT (dynamic JSON with request fields)
- `/api/compute` - AOT (fibonacci + dynamic JSON)
- `/api/greet/:name` - AOT (prefix match + template)
- `/` - Interpreter (JSX)
- `/api/json` - Interpreter (JSON.tryParse)
- 404 fallback - AOT (static JSON with dynamic URL)

Target: 5 of 7 routes fully transpiled.

---

## File Inventory

### New Files
| File | Checkpoint | Purpose |
|---|---|---|
| `tools/transpiler.zig` | 1 | IR-to-Zig source transpiler |
| `docs/ts-to-zig-transpiler.md` | 0 | This plan document |

### Modified Files
| File | Checkpoint | Changes |
|---|---|---|
| `tools/precompile.zig` | 1,2 | Invoke transpiler, fall back to pattern matcher |
| `build.zig` | - | No changes needed (already has `-Daot`) |
| `src/zruntime.zig` | - | No changes needed (already has AOT dispatch) |
| `src/embedded_handler_stub.zig` | - | No changes needed |

### Preserved Files (No Changes)
| File | Reason |
|---|---|
| `zts/handler_analyzer.zig` | Kept as fallback until transpiler reaches parity |
| `zts/parser/ir.zig` | Read-only input to transpiler |
| `zts/bytecode.zig` | Bytecode still generated for interpreter fallback |

---

## Transpiler Module Design

### `tools/transpiler.zig` Structure

```
IrTranspiler
  .ir: IrView                    -- Input IR
  .output: ArrayList(u8)         -- Generated Zig source
  .indent: u16                   -- Current indentation level
  .locals: AutoHashMap(u16, LocalInfo) -- Binding slot -> local variable info
  .functions: ArrayList(FunctionInfo)  -- Discovered functions to transpile
  .type_env: TypeEnvironment     -- Inferred types per binding

  // Entry points
  .transpileHandler(root) !void  -- Transpile full handler file
  .transpileFunction(fn_node) !void -- Transpile one function

  // Node transpilation (recursive descent)
  .transpileNode(idx) !void      -- Dispatch on NodeTag
  .transpileExpr(idx) !void      -- Expression nodes
  .transpileStmt(idx) !void      -- Statement nodes

  // Type inference
  .inferExprType(idx) ?ZigType   -- Bottom-up type inference
  .inferFunctionTypes(fn_node) ?FunctionSig -- Parameter/return types

  // Helpers
  .emit(str) void                -- Write to output buffer
  .emitIndent() void             -- Write current indentation
  .pushIndent() void             -- Increase indent
  .popIndent() void              -- Decrease indent
  .localName(slot) []const u8    -- "local_N" for slot N
```

### TypeEnvironment

Tracks inferred types during function analysis:

```zig
const ZigType = enum {
    i32_type,      // Integer operations only
    f64_type,      // Float operations
    bool_type,     // Boolean
    string_type,   // []const u8
    jsvalue_type,  // Dynamic, boxed
    void_type,     // No value
};

const LocalInfo = struct {
    name: []const u8,      // "local_0", "arg_0", etc.
    zig_type: ZigType,     // Inferred type
    is_mutable: bool,      // let vs const
    is_param: bool,        // Function parameter
};

const FunctionSig = struct {
    params: []ZigType,
    return_type: ZigType,
    is_pure_integer: bool, // All operations are integer
};
```

---

## Verification Strategy

### Differential Testing
For every checkpoint, run both paths and compare:

```bash
# Interpreter only
zig build run -Dhandler=examples/handler.jsx -- -p 3001 &
# AOT + interpreter fallback
zig build run -Dhandler=examples/handler.jsx -Daot=true -- -p 3002 &

# Compare responses
for url in /api/health /api/echo /api/compute "/api/greet/world"; do
  diff <(curl -s localhost:3001$url) <(curl -s localhost:3002$url) || echo "MISMATCH: $url"
done
```

### Unit Tests
Each transpiler function gets unit tests using the IR builder:

```zig
test "transpile integer literal" {
    var store = IRStore.init(testing.allocator);
    defer store.deinit();
    const idx = try store.addLitInt(loc, 42);
    var t = IrTranspiler.init(testing.allocator, IrView.fromIRStore(&store, &pool));
    defer t.deinit();
    try t.transpileExpr(idx);
    try testing.expectEqualStrings("42", t.output.items);
}
```

### Build Verification
Every checkpoint must pass:
```bash
zig build                                    # Debug build
zig build test                               # All tests
zig build test-zts                           # Engine tests
zig build -Dhandler=examples/handler.jsx -Daot=true  # AOT build
zig build -Dhandler=examples/handler.ts -Daot=true   # TS AOT build
```

### Benchmark
```bash
zig build bench -Dhandler=examples/handler.jsx -Daot=true -Doptimize=ReleaseFast
```

Compare per-route latency between interpreter and AOT paths. Target: 30-50% reduction for transpiled routes.
