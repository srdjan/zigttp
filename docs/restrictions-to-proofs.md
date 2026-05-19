# Restrictions to Proofs

This document is generated from `packages/tools/src/json_diagnostics.zig`.
It maps each zigts language restriction to the failure class it eliminates
and the proof it unlocks. Run `zigts restrictions` for the live table.

Every entry is a deliberate cut from JavaScript or TypeScript that buys
a specific soundness guarantee. The intent oracle (`zigts assert-intent`)
and the contract diff (`zigttp proofs show`) live above these cuts; the
cuts themselves are what make those higher-level claims possible.

The proof card's `Trade` lens in `zigttp dev` (press `Tab` to rotate)
and the matching tab in Studio render a per-property view of this table
against the current handler, so you can see exactly which restrictions
earned each `[+]` chip.

| Restriction | Failure class prevented | Proof unlocked | Alternative |
|-------------|-------------------------|----------------|-------------|
| `switch/case` | non-exhaustive control flow and implicit fallthrough | match coverage and exhaustive return analysis | use 'match' expression |
| `var` | scope hoisting and temporal dead zones | block-scoped data flow and reachability analysis | use 'let' or 'const' |
| `class` | implicit mutable receivers and hidden state | explicit data flow and effect analysis | use plain objects and functions |
| `while` | unbounded back-edges and non-termination | finite path enumeration and termination | use 'for...of' with a finite collection |
| `do...while` | unbounded back-edges and non-termination | finite path enumeration and termination | use 'for...of' with a finite collection |
| `for(;;)` | unbounded back-edges and non-termination | finite path enumeration and termination | use 'for (const i of range(n))' |
| `for...in` | prototype-chain iteration and non-deterministic order | deterministic iteration and shape-stable access | use 'for (const k of Object.keys(obj))' |
| `try/catch` | hidden exceptional control flow | Result narrowing and exhaustive path enumeration | use Result types and check .ok |
| `throw` | hidden exceptional control flow | Result narrowing and exhaustive return analysis | return an error Response |
| `async/await` | ambient scheduling and non-deterministic interleavings | deterministic effect boundary and replayable I/O | use fetchSync(), parallel(), race() |
| `new` | constructor dispatch and hidden initialization effects | explicit factory call sites and visible effects | use factory functions or object literals |
| `this` | dynamic receiver binding | static call-graph and visible data flow | use explicit parameter passing |
| `null` | dual absent-value sentinels | optional-narrowing proof totality | use undefined |
| `== / !=` | implicit coercion paths | sound type-directed comparison | use === / !== |
| `++ / --` | hidden in-place mutation in expressions | explicit assignment effects and state isolation | use x = x + 1 |
| `regex` | opaque accept set and catastrophic backtracking | shape-checkable validation via zigttp:validate schemas | use string methods (includes, startsWith, etc.) |
| `delete` | hidden-class shape mutation | shape-stable property access | use destructuring: const { key, ...rest } = obj |
| `enum` | dual numeric/string lookup and non-exhaustive cases | exhaustive match coverage on discriminated unions | use object literals or discriminated unions |
| `decorator (@)` | implicit metaprogramming and target rewriting | static call-graph and visible effect composition | use function composition |
| `namespace` | module-graph blind spots | AST-driven contract extraction | use ES6 modules |

## Why

Per-restriction rationale. Each line answers "why is this cut worth
making?" in one sentence.

- **`switch/case`** - fallthrough makes coverage ambiguous and lets cases share state through implicit fallthrough.
- **`var`** - hoisting and function-scoping create temporal dead zones the verifier cannot reason about.
- **`class`** - implicit mutable receivers hide data flow from the contract extractor.
- **`while`** - unbounded back-edges defeat finite path enumeration.
- **`do...while`** - unbounded back-edges defeat finite path enumeration.
- **`for(;;)`** - C-style loops carry no bound; gen-tests cannot enumerate every iteration.
- **`for...in`** - for...in walks the prototype chain; iteration order is implementation-defined.
- **`try/catch`** - exceptions are an invisible second return channel that bypasses the type system.
- **`throw`** - throw is the producer side of the hidden exception channel.
- **`async/await`** - ambient scheduling produces interleavings the replay log cannot reproduce.
- **`new`** - constructor dispatch combined with prototypes hides effects from the IR.
- **`this`** - the binding of `this` is dynamic and unreadable from the IR.
- **`null`** - two absent-value sentinels split optional narrowing into two incompatible lattices.
- **`== / !=`** - loose equality coerces operands, creating control-flow paths the type checker cannot see.
- **`++ / --`** - in-place mutation hides write effects in expression positions.
- **`regex`** - regex literals describe an opaque accept set the validator cannot reason about.
- **`delete`** - delete mutates hidden-class shape, defeating shape-stable property access.
- **`enum`** - TS enums emit dual numeric/string lookups that bypass exhaustive match checking.
- **`decorator (@)`** - decorators rewrite their target at runtime in ways the contract extractor cannot trace.
- **`namespace`** - TS namespaces compile to closures with mutable internals invisible to the module graph.
