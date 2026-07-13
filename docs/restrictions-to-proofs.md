# Restrictions to Proofs

This document is generated from `packages/tools/src/json_diagnostics.zig`.
It maps each zigts language restriction to the failure class it eliminates
and the proof it unlocks. Run `zigts restrictions` for the live table.

Every entry is a deliberate cut from JavaScript or TypeScript that buys
a specific soundness guarantee. The author-declared intent assertions
(extracted with `-Dcontract`) and the contract diff (`zigttp proofs show`)
live above these cuts; the cuts themselves are what make those
higher-level claims possible.

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
| `delete` | hidden-class shape mutation | shape-stable property access | build a new object literal with only the keys you keep |
| `enum` | dual numeric/string lookup and non-exhaustive cases | exhaustive match coverage on discriminated unions | use object literals or discriminated unions |
| `decorator (@)` | implicit metaprogramming and target rewriting | static call-graph and visible effect composition | use function composition |
| `namespace` | module-graph blind spots | AST-driven contract extraction | use ES6 modules |
| implicit unknown call result | untyped helper boundaries | typed call graph for expert repair | annotate local helpers or use modeled virtual modules |
| missing named-function annotations | inferred public contracts | stable handler and helper signatures | add parameter and return types |
| dynamic capability key | hidden env/cache/sql/egress authority | literal capability manifests | use string literals or const literal aliases |
| avoidable `let` | needless mutable slots | simpler state-isolation proof | use `const` unless reassigned |
| dynamic computed property access | shape-erasing indexed reads | shape-stable property access | use fields, literal keys, or const literal aliases |
| ternary `a ? b : c` | duplicate branching idiom and inline-nesting drift | one canonical branching form per control-flow shape | use `if`/`else` or `match` |
| compound assignment (`+=`, `-=`, ...) | hidden mutation in expression position | explicit write effects and diff-visible updates | write `x = x + e` |
| non-leading object spread | precedence ambiguity around "which keys win" | unambiguous override semantics | put spread first: `{...base, x: 1}` |
| complex template interpolation | inline expressions hide call/effect sites in strings | named intermediates and visible call sites | hoist into a `const` above the template |
| call-site spread `f(...args)` | dynamic arity invisible to the contract extractor | static call-arity and stable helper contracts | pass positional args or widen the helper signature |
| default parameter value | hidden default invisible at call sites and in contracts | diff-visible defaults in the function body | accept `T \| undefined` and resolve in the body |
| nested destructuring | deep patterns inflate review cost and drift in agent output | one-level destructure with intermediate names | drill in with follow-up `const` bindings |
| unused index alias in `for...of` | alias-tracking pass on a binding that is never read | iterator-scope confinement with one fewer binding to track | iterate over the array directly; drop `.entries()` and the destructure |
| boolean compared to a boolean literal (`x === true`) | redundant spelling of the boolean test, with a non-boolean identity-comparison footgun | one canonical boolean-test shape; flow/narrowing passes skip literal-comparison nodes | use the boolean directly: `x` (or `!x` for `=== false`) |

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
- **implicit unknown call result** - unknown return types hide control-flow and data-flow facts from the checker.
- **missing named-function annotations** - public helper boundaries need stable signatures for repeatable repair.
- **dynamic capability key** - non-literal authority cannot be represented in a precise deployment contract.
- **avoidable `let`** - unnecessary mutable slots widen the state the verifier must track.
- **dynamic computed property access** - arbitrary indexes erase object shape and defeat field-level narrowing.
- **ternary `a ? b : c`** - duplicate branching idiom that competes with `if`/`else` and `match`; nested ternaries are a primary source of agent style drift.
- **compound assignment (`+=`, `-=`, ...)** - in-place arithmetic update hides write effects in expression position, the same footgun the `++`/`--` ban already addressed.
- **non-leading object spread** - mixed spread-and-literal order makes the "which keys win" reading ambiguous; the leading-spread form has only one valid reading.
- **complex template interpolation** - function calls and arithmetic inside `${...}` hide call sites in strings; named intermediates surface them.
- **call-site spread `f(...args)`** - dynamic arity defeats the contract extractor's ability to pin a helper's parameter shape.
- **default parameter value** - signature-site defaults are invisible to the contract extractor and to callers reading the diff.
- **nested destructuring** - patterns nested inside another binding pattern inflate review cost without buying any proof.
- **unused index alias in `for...of`** - an index binding the loop body never reads still forces the analyzer to track it before proving it dead; dropping `.entries()` lets iterator-scope confinement land directly.
