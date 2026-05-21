# Language Subset

zigts runs a restricted JavaScript and TypeScript profile. The restrictions are
part of the product surface: they make verification, deterministic replay,
contract extraction, and generated tests tractable.

## Supported Runtime Syntax

- `let` and `const`
- Function declarations and arrow functions
- Object and array literals
- Destructuring and spread
- Template literals
- `if` / `else`
- `for...of` over finite collections, including `break` and `continue`
- Strict equality: `===` and `!==`
- Optional chaining and nullish coalescing
- Compound arithmetic and bitwise assignment
- Pipe operator, `value |> transform`
- `match` expressions
- `assert` statements
- JSON, Math, console, and selected string/array/object helpers
- JSX and TSX when the file extension enables JSX parsing

## Supported TypeScript Surface

- Type aliases and generic type aliases
- `distinct type`
- Interfaces
- Variable, parameter, and return annotations
- Function generics
- `import type` / `export type`
- `Spec<T>`, `Proof<T, S>`, and `Effects<T, S>` from `zigttp:types`
- Type guards with `x is T`
- Readonly fields
- Template literal types

Type annotations are stripped before runtime execution, then checked by the
compiler pipeline.

## Not Supported

Unsupported features fail at parse or strip time with a suggestion.

| Feature | Use Instead |
|---|---|
| `var` | `let` or `const` |
| `class`, `this`, `new`, `super` | Plain objects, factory functions, explicit parameters |
| `while`, `do...while`, C-style `for` | `for...of` over a finite collection or `range(n)` |
| `for...in` | `Object.keys()` with `for...of` |
| `switch` | `match` |
| `try`, `catch`, `throw` | `Result` values and checked branches |
| `async`, `await`, `Promise` | `zigttp:io` `parallel()` / `race()` or synchronous module APIs |
| `null` | `undefined` |
| `==`, `!=` | `===`, `!==` |
| `++`, `--` | `x = x + 1`, `x = x - 1` |
| Regular expressions | String helpers or `zigttp:validate` schemas |
| `delete` | Object rest destructuring |
| TypeScript `any` | Specific types or unions |
| Type assertions with `as` / `satisfies` | Control-flow narrowing or explicit annotations |

## Why These Cuts Exist

Each restriction maps to a proof benefit. Examples:

- No unbounded loops means path enumeration can terminate.
- No exceptions means every failure path is visible in ordinary control flow.
- No classes or `this` means data flow stays explicit.
- One absent sentinel, `undefined`, keeps optional narrowing simple.
- Literal capability keys let contracts list env vars, SQL queries, cache
  namespaces, and egress hosts precisely.

See [Restrictions to Proofs](restrictions-to-proofs.md) for the full generated
mapping and [Feature Detection](feature-detection.md) for the detection matrix.
