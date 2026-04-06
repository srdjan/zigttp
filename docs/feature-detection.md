# Feature Detection Matrix

This document catalogs all unsupported JavaScript and TypeScript features detected by zigttp's fail-fast validation system, organized by detection layer.

## Detection Architecture

zigttp uses a two-layer validation system:

1. **TypeScript Stripper** (`zigts/stripper.zig`): Runs first for .ts/.tsx files, catches TypeScript-specific syntax that only exists in type positions
2. **Parser** (`zigts/parser/parse.zig`): Runs for all files (after stripping for TS), catches unsupported JavaScript and TypeScript features

**Principle**: Each feature should be detected at exactly one layer to avoid duplicate error reporting and ensure consistent error messages regardless of file type.

## TypeScript Stripper Features

These features exist only in TypeScript type annotation positions that are stripped before parsing. The parser never sees them.

| Feature | Error Type | Suggested Alternative |
|---------|------------|----------------------|
| `any` type (all positions: annotations, assertions, nested) | UnsupportedAnyType | Use specific types (string, number, object) or union types |
| `as` type assertion (e.g., `x as string`) | UnsupportedAssertion | Use explicit type narrowing with typeof guards or undefined checks |
| `satisfies` operator (e.g., `x satisfies T`) | UnsupportedAssertion | Use explicit type annotations on declarations |

## Supported Module Syntax

The parser supports ES6 `import`/`export` syntax for the virtual module system (`zigttp:*`).

### Supported Import Forms

| Syntax | Description |
|--------|-------------|
| `import { x } from "zigttp:env"` | Named import (single) |
| `import { x, y, z } from "zigttp:crypto"` | Named import (multiple) |
| `import { x as alias } from "zigttp:env"` | Named import with alias |
| `import { parseBearer, jwtVerify } from "zigttp:auth"` | Auth module imports |
| `import { schemaCompile, validateJson } from "zigttp:validate"` | Validation module imports |
| `import { decodeJson, decodeForm } from "zigttp:decode"` | Typed ingress module imports |
| `import { cacheGet, cacheSet } from "zigttp:cache"` | Cache module imports |
| `import { run, step } from "zigttp:durable"` | Durable execution imports |

### Supported Export Forms

| Syntax | Description |
|--------|-------------|
| `export function handler(req) {}` | Named function export |
| `export const version = "1.0"` | Named const export |
| `export let count = 0` | Named let export |

### Unsupported Module Forms

These produce helpful error messages directing users to named imports/exports:

| Syntax | Error Message |
|--------|---------------|
| `import X from "mod"` | Default imports not supported; use named imports |
| `import * as X from "mod"` | Namespace imports not supported; use named imports |
| `import "mod"` | Side-effect imports not supported; use named imports |
| `export default function() {}` | Export default not supported; use named exports |
| `export { x } from "mod"` | Re-exports not supported; use named exports |
| `export * from "mod"` | Export star not supported; use named exports |

## Parser Features (54 total)

These are JavaScript and TypeScript language features that are syntactically valid but unsupported in zigttp's runtime. All are detected during parsing with helpful error messages following the pattern: "'feature' is not supported; use X instead".

### TypeScript Features (detected by parser)

These were previously detected by the stripper but moved to the parser for consistent error messages across .ts and .js files.

| Feature | Suggested Alternative |
|---------|----------------------|
| `enum` / `const enum` | Use object literals or discriminated unions |
| `namespace` / `module` | Use ES6 modules |
| `implements` keyword | Use duck typing or runtime checks |
| `@decorator` syntax | Use function composition |
| Access modifiers (`public`, `private`, `protected`) | Use naming conventions (e.g., `_private`) |

### Loop Constructs

| Feature | Suggested Alternative |
|---------|----------------------|
| `while` loops | Use `for-of` with a finite collection |
| `do-while` loops | Use `for-of` with a finite collection |
| `for-in` loops | Use `for-of` to iterate over values |
| C-style `for` loops (init; cond; update) | Use `for (let x of array)` or `for (let i of range(n))` |

### Loop Control Flow

| Feature | Status |
|---------|--------|
| `break` in `for-of` | Supported |
| `continue` in `for-of` | Supported |
| `break` outside loop | Error: "'break' outside of loop or switch" |
| `continue` outside loop | Error: "'continue' outside of loop" |
| Labeled `break`/`continue` | Error: use a conditional instead |

### Error Handling

| Feature | Suggested Alternative |
|---------|----------------------|
| `throw` statement | Use Result types for error handling |
| `try/catch/finally` | Use Result types for error handling |

### Control Flow Statements

| Feature | Suggested Alternative |
|---------|----------------------|
| `switch/case/break` | Use `match` expression instead |

### Classes and OOP

| Feature | Suggested Alternative |
|---------|----------------------|
| `class` declarations (statement context) | Use plain objects and functions |
| `class` expressions (expression context) | Use plain objects and functions |
| `this` keyword | Pass context explicitly as a parameter |
| `super` keyword | Use explicit function calls |
| `new` operator | Use factory functions |

### Variable Declarations

| Feature | Suggested Alternative |
|---------|----------------------|
| `var` keyword (statement context) | Use `let` or `const` |
| `var` keyword (for-loop context) | Use `let` or `const` |

### Equality Operators

| Feature | Suggested Alternative |
|---------|----------------------|
| `==` (loose equality) | Use `===` for strict equality |
| `!=` (loose inequality) | Use `!==` for strict inequality |

### Unary Increment/Decrement

| Feature | Suggested Alternative |
|---------|----------------------|
| `++x` (prefix increment) | Use `x = x + 1` |
| `--x` (prefix decrement) | Use `x = x - 1` |

### Postfix Increment/Decrement

| Feature | Suggested Alternative |
|---------|----------------------|
| `x++` (postfix increment) | Use `x = x + 1` |
| `x--` (postfix decrement) | Use `x = x - 1` |

### Supported Compound Assignment Operators (12 total)

Arithmetic and bitwise compound assignments are supported and desugar to `x = x [op] value`:

`+=`, `-=`, `*=`, `/=`, `%=`, `**=`, `&=`, `|=`, `^=`, `<<=`, `>>=`, `>>>=`

### Unsupported Logical Compound Assignments (3 total)

Logical compound assignments require short-circuit semantics and are not supported:

| Feature | Suggested Alternative |
|---------|----------------------|
| `&&=` | `x = x && value` |
| `\|\|=` | `x = x \|\| value` |
| `??=` | `x = x ?? value` |

### Type-checking Operator

| Feature | Suggested Alternative |
|---------|----------------------|
| `instanceof` | Use discriminated unions with tag property |

### Values

| Feature | Suggested Alternative |
|---------|----------------------|
| `null` | Use `undefined` for absent values |

### Expression-level Features

| Feature | Suggested Alternative |
|---------|----------------------|
| Regular expressions `/.../ ` | Use string methods |
| Function expressions (named & anonymous) | Use arrow functions `(x) => x * 2` or function declarations |
| `yield` expressions | Generators are not available |
| `delete` operator | Use object spread to omit properties |

### Global Identifiers

| Feature | Suggested Alternative |
|---------|----------------------|
| `Promise` (as unbound global) | Use Result types or callbacks |
| `RegExp` (as unbound global) | Use string methods |

### Object Built-in Methods

| Feature | Suggested Alternative |
|---------|----------------------|
| `Object.assign()` | Use object spread `{...obj1, ...obj2}` |
| `Object.freeze()` | Objects are mutable by design |
| `Object.isFrozen()` | Objects are mutable by design |

## Error Message Pattern

All error messages follow a consistent format:

```
"'<feature>' is not supported; use <alternative> instead"
```

Examples:
- `'class' is not supported; use plain objects and functions instead`
- `'while' loops are not supported; use 'for-of' with a finite collection instead`
- `'throw' is not supported; use Result types for error handling instead`
- `'enum' is not supported; use object literals or discriminated unions instead`

## Adding New Unsupported Features

When adding detection for a new unsupported feature:

1. **Determine Layer**:
   - TypeScript type-position-only syntax (e.g., `any` type) -> Stripper
   - Everything else (JS features, TS keywords that exist as statements) -> Parser

2. **Add Detection Code**:
   - Follow existing error reporting pattern for that layer
   - Include helpful alternative in error message
   - Preserve source location for accurate error reporting

3. **Add Tests**:
   - Stripper: Add test in `zigts/stripper.zig` test section
   - Parser: Add test in `zigts/parser/parse.zig` test section
   - Verify error message content, not just error type

4. **Update This Document**:
   - Add row to appropriate table
   - Update count if adding to parser features

5. **Run Tests**:
   - `zig build test` should pass
   - Manually verify error message quality with test input

## Design Rationale

### Why Two Layers?

**TypeScript Stripper**: Handles TypeScript syntax that exists only in type annotation positions (e.g., `any` type). These are stripped before parsing, so the parser never sees them.

**Parser**: Handles all other feature detection - both JavaScript features and TypeScript keywords that appear as statements (enum, namespace, implements, decorators, access modifiers). Running detection in the parser ensures consistent error reporting for both .ts and .js files.

### Why Not Runtime Detection?

Fail-fast at parse time provides:
- Immediate feedback to developers
- Prevents invalid bytecode generation
- Clearer error messages with source context
- No runtime performance overhead

Runtime checks should only exist as defensive programming (e.g., `UnimplementedOpcode`), not primary feature detection.

### Why Move Detection to the Parser?

Before consolidation, features like `class` and `enum` were detected in the stripper for .ts files but only in the parser for .js files. This created inconsistent developer experience based on file extension. Moving all keyword-level detection to the parser ensures all developers see the same helpful error with rich formatting (source context, underlines) regardless of file type.

The features remaining in the stripper are `any` type detection (because `any` only appears in type annotation positions that are stripped before the parser runs) and `as`/`satisfies` assertion rejection (because these are type-position syntax that the parser never sees).

