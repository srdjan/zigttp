# JS Subset Feature Matrix

A cross-reference between the documented JS/TS subset and the engine layers
that implement it: parser, IR, bytecode codegen, interpreter, baseline JIT,
optimized JIT. The matrix surfaces features that are documented as supported
but lack end-to-end coverage at one or more layers, so a gap is easy to spot
before a user trips on it.

This is a discovery deliverable. Entries marked **TODO-verify** were
inferred from code locations and need a corresponding test fixture pin
before they can be promoted to **OK**. Entries marked **none** at a tier
mean the tier intentionally does not implement the feature (e.g., JIT does
not need to handle import resolution — that runs at compile time).

Sources used to populate this matrix:

- `docs/language-subset.md` — authoritative list of supported syntax.
- `docs/feature-detection.md` — features that fail at parse/strip with a
  suggestion, including the parser detection layer responsible for each.
- `packages/zigts/src/bytecode.zig` — opcode enum (`Opcode`).
- `packages/zigts/src/parser/parse.zig`, `parser/ir.zig`, `parser/codegen.zig`.
- `packages/zigts/src/interpreter.zig` — main dispatch switch.
- `packages/zigts/src/interpreter/baseline.zig` — baseline JIT codegen.
- `packages/zigts/src/jit/optimized.zig` — optimized JIT codegen.
- `examples/**/*.test.jsonl` — handler-level fixtures.

## How to read the columns

- **Parser**: feature accepted by the parser without a diagnostic.
- **IR**: feature represented as a distinct IR node or attribute.
- **Codegen**: bytecode emitted by `parser/codegen.zig` for the feature.
- **Interp**: opcode handled in the interpreter dispatch switch.
- **Baseline JIT**: opcode supported by `baseline.zig`. A miss falls back to
  interpreter at function call boundary.
- **Optimized JIT**: opcode supported by `optimized.zig`. A miss deopts to
  baseline.
- **Test fixture**: the most-precise existing fixture path. **gap** means
  no fixture pins this feature end-to-end at handler level.

## Runtime syntax

| Feature | Parser | IR | Codegen | Interp | Baseline JIT | Optimized JIT | Test fixture |
|---|---|---|---|---|---|---|---|
| `let` declarations | OK | OK | `put_loc*` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| `const` declarations | OK | OK | `put_loc*` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| Function declarations | OK | OK | `make_function` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| Arrow functions | OK | OK | `make_closure` | OK | TODO-verify | TODO-verify | examples/handler/sugar.test.jsonl |
| Object literals | OK | OK | `new_object_literal`, `set_slot` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| Array literals | OK | OK | `new_array` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| Destructuring (object, array) | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | examples/handler/sugar.test.jsonl |
| Spread in arrays / calls | OK | OK | `array_spread`, `call_spread` | OK | TODO-verify | TODO-verify | gap |
| Template literals | OK | OK | `concat_n`, `concat_2` | OK | TODO-verify | TODO-verify | examples/handler/sugar.test.jsonl |
| `if` / `else` | OK | OK | `if_true`, `if_false`, `goto` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| `for...of` over arrays | OK | OK | `for_of_next`, `for_of_next_put_loc` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| `for...of` over `range(n)` | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| `break` / `continue` in `for...of` | OK | OK | `goto`, `drop_goto` | OK | TODO-verify | TODO-verify | gap |
| Strict equality `===` / `!==` | OK | OK | `strict_eq`, `strict_neq` | OK | TODO-verify | TODO-verify | examples/handler/handler.test.jsonl |
| Optional chaining `?.` | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| Nullish coalescing `??` | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| Compound arithmetic `+=`, `*=`, etc. | OK | OK | `add_const_i8` etc. | OK | TODO-verify | TODO-verify | gap |
| Compound bitwise `&=`, `|=`, `^=`, `<<=`, `>>=` | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| Pipe operator `\|>` | OK (parse.zig:4327) | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| `match` expression | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | examples/routing/match-handler.test.jsonl |
| `assert` statement | OK (parse.zig:4074) | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| JSX / TSX | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | examples/jsx/jsx-ssr.test.jsonl |
| Closures (capture by reference) | OK | OK | `make_closure`, `get_upvalue`, `put_upvalue`, `close_upvalue` | OK | TODO-verify | TODO-verify | gap |
| Tail call `return f(...)` | OK | OK | `tail_call` | OK | TODO-verify | TODO-verify | gap |

## Standard library helpers

| Feature | Parser | IR | Codegen | Interp | Baseline JIT | Optimized JIT | Test fixture |
|---|---|---|---|---|---|---|---|
| `Object.keys` / `Object.values` / `Object.entries` | OK | OK | `get_global`, `call` | OK | TODO-verify | TODO-verify | gap |
| `Math.floor` / `ceil` / `round` / `abs` | OK | OK | `math_floor`, `math_ceil`, `math_round`, `math_abs` | OK | TODO-verify | TODO-verify | gap |
| `Math.min` / `Math.max` (2-arg) | OK | OK | `math_min2`, `math_max2` | OK | TODO-verify | TODO-verify | gap |
| `console.log` | OK | OK | TODO-verify | OK | none | none | examples/handler/handler.test.jsonl |
| `JSON.parse` / `JSON.stringify` | OK | OK | TODO-verify | OK | none | none | examples/handler/handler.test.jsonl |
| Array HOFs (`.map`, `.filter`, `.reduce`, `.forEach`) | OK | OK | `call_method` | OK | TODO-verify | TODO-verify | gap |
| `range(n)` | OK | OK | TODO-verify | OK | TODO-verify | TODO-verify | gap |
| Typed arrays | OK | TODO-verify | TODO-verify | TODO-verify | TODO-verify | TODO-verify | gap |

## TypeScript surface (stripped before runtime, then checked by analyzer)

| Feature | Stripper | Type checker | Type pool | Codegen | Test fixture |
|---|---|---|---|---|---|
| Type aliases | OK | OK | `type_pool.zig` | strip-only | examples/handler/handler-ts.test.jsonl |
| Generic type aliases | OK | OK | `type_pool.zig` | strip-only | examples/handler/handler-ts.test.jsonl |
| `distinct type` | OK | OK | `type_pool.zig` | strip-only | gap |
| Interfaces | OK | OK | `type_pool.zig` | strip-only | gap |
| Variable / parameter / return annotations | OK | OK | `type_pool.zig` | strip-only | examples/handler/handler-ts.test.jsonl |
| Function generics | OK | OK | `type_pool.zig` | strip-only | gap |
| `import type` / `export type` | OK | OK | `type_pool.zig` | strip-only | examples/handler/handler-ts.test.jsonl |
| `Spec<T>`, `Proof<T,S>`, `Effects<T,S>` from `zigttp:types` | OK | OK | `type_pool.zig` (t_intersection) | strip-only | gap |
| Type guards `x is T` | OK | OK | `type_pool.zig` | strip-only | gap |
| Readonly fields | OK | OK | `type_pool.zig` | strip-only | gap |
| Template literal types | OK | OK | `type_pool.zig` (matchesTemplateLiteral, type_pool.zig:1934) | strip-only | gap |

## Unsupported (rejected at parse/strip with suggestion)

`docs/feature-detection.md` is the authoritative table. Spot checks:

| Feature | Layer | Suggestion |
|---|---|---|
| `var` | parser | `let` or `const` |
| `class`, `this`, `new`, `super` | parser | Plain objects, factory functions |
| `while`, `do...while`, C-style `for` | parser | `for...of` over a finite collection or `range(n)` |
| `for...in` | parser | `Object.keys()` with `for...of` |
| `switch` | parser | `match` |
| `try`, `catch`, `throw` | parser | Result values and checked branches |
| `async`, `await`, `Promise` | parser | `zigttp:io` `parallel()` / `race()` |
| `null` | parser | `undefined` |
| `==`, `!=` | parser | `===`, `!==` |
| `++`, `--` | parser | `x = x + 1`, `x = x - 1` |
| Regex literals | parser | String helpers or `zigttp:validate` schemas |
| `delete` | parser | Object rest destructuring |
| `any`, `as`, `satisfies` | stripper | Specific types or unions, explicit narrowing |
| `enum`, `namespace`, `module`, decorators, access modifiers | parser | ES6 modules, naming conventions |

## Identified gaps

These features appear in `docs/language-subset.md` as supported but the
matrix shows **gap** in the Test fixture column. Promoting them to **OK**
requires either pointing at an existing fixture I missed or adding a
small handler that exercises the feature and a `.test.jsonl` next to it.

- Spread in array literals and call arguments
- `for...of` over `range(n)` (range itself is documented; `for...of` over
  range is the composition that matters)
- `break` / `continue` inside `for...of`
- Optional chaining `?.`
- Nullish coalescing `??`
- Compound arithmetic and bitwise assignments
- Pipe operator `|>`
- `assert` statement
- Closures with upvalue capture
- Tail calls
- `Object.keys` / `values` / `entries` end-to-end (handler scope)
- `Math.*` end-to-end
- Array HOFs (`map`, `filter`, `reduce`, `forEach`)
- Typed arrays
- `distinct type`, interfaces, generic functions, type guards, readonly
  fields, template literal types — TS surface, mostly strip-only but
  unobserved at handler scope

## JIT tier coverage

The matrix marks most opcode-level rows as **TODO-verify** for baseline
and optimized JIT. The next pass should walk `interpreter/baseline.zig`
and `jit/optimized.zig` opcode-by-opcode and update the columns. Two
quick principles to apply during that walk:

1. Pure-function builtins (Math.*, JSON.*, Object helpers, string slice
   and indexOf, parseInt and parseFloat) ride the interpreter's bypass
   path in `interpreter/call.zig` and intentionally never enter JIT.
   Mark them **none** at the JIT columns; they are correct as-is.
2. The optimized JIT uses type-specialized opcodes (`add_num`, `sub_num`,
   `mul_num`, `lt_num` and friends, `concat_2`). If a feature emits the
   generic opcode (`add`, `lt`, `concat_n`), the optimized tier needs
   the type feedback to specialize. A "no specialization observed in
   feedback" row is also valuable — it surfaces features whose perf is
   left on the table.

## Maintenance

This matrix is doc-only; it does not gate the build. To keep it honest:

- When adding a documented language feature, add a row here and at least
  one fixture path in `examples/`.
- When adding an opcode, list which feature it supports and link to the
  fixture that pins it.
- When a feature is intentionally dropped, move its row to the
  Unsupported section with the suggested alternative.
