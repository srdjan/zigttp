# Feature Detection Matrix

This document catalogs all unsupported JavaScript and TypeScript features detected by zigttp's fail-fast validation system, organized by detection layer.

## Detection Architecture

zigttp uses a two-layer validation system:

1. **TypeScript Stripper** (`zts/stripper.zig`): Runs first for .ts/.tsx files, catches TypeScript-specific syntax
2. **Parser** (`zts/parser/parse.zig`): Runs for all files (after stripping for TS), catches unsupported JavaScript features

**Principle**: Each feature should be detected at exactly one layer to avoid duplicate error reporting and ensure consistent error messages regardless of file type.

## TypeScript Stripper Features

These features are TypeScript-specific and have no JavaScript equivalent. They are detected during the stripping phase before parsing.

| Feature | Line | Error Type | Suggested Alternative |
|---------|------|------------|----------------------|
| `enum` / `const enum` | 1002-1014 | UnsupportedEnum | Use object literals or discriminated unions |
| `namespace` / `module` | 1016-1017 | UnsupportedNamespace | Use ES6 modules |
| ~~`class`~~ | ~~1020-1022~~ | ~~UnsupportedClass~~ | **MOVED TO PARSER** (Stage 3) |
| ~~`abstract class`~~ | ~~1023-1033~~ | ~~UnsupportedClass~~ | **MOVED TO PARSER** (Stage 3) |
| `implements` | 1034-1035 | UnsupportedClass | Use duck typing or runtime checks |
| `@decorator` syntax | 995-996 | UnsupportedDecorator | Use function composition |
| Access modifiers (`public`, `private`, `protected`) | 1038-1057 | UnsupportedClass | Use naming conventions (e.g., `_private`) |
| `any` type (all positions: annotations, assertions, nested) | 1057-1072 | UnsupportedAnyType | Use specific types (string, number, object) or union types |

**Note**: After Stage 3 implementation, `class` and `abstract class` will be handled by the parser to ensure consistent error messages for both .ts and .js files.

## Supported Module Syntax

The parser supports ES6 `import`/`export` syntax for the virtual module system (`zigttp:*`).

### Supported Import Forms

| Syntax | Description |
|--------|-------------|
| `import { x } from "zigttp:env"` | Named import (single) |
| `import { x, y, z } from "zigttp:crypto"` | Named import (multiple) |
| `import { x as alias } from "zigttp:env"` | Named import with alias |

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

## Parser Features (48 total)

These are JavaScript language features that are syntactically valid but unsupported in zigttp's runtime. All are detected during parsing with helpful error messages following the pattern: "'feature' is not supported; use X instead".

### Loop Constructs

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `while` loops | 170-177 | Use `for-of` with a finite collection |
| `do-while` loops | 170-177 | Use `for-of` with a finite collection |
| `for-in` loops | 862-864 | Use `for-of` to iterate over values |
| C-style `for` loops (init; cond; update) | 915-918 | Use `for (let x of array)` or `for (let i of range(n))` |

### Loop Control Flow

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `break` statement | 180-186 | Use early return or filter |
| `continue` statement | 180-186 | Use filter or conditional logic |

### Error Handling

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `throw` statement | 188-194 | Use Result types for error handling |
| `try/catch/finally` | 188-194 | Use Result types for error handling |

### Classes and OOP

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `class` declarations (statement context) | 197-198 | Use plain objects and functions |
| `class` expressions (expression context) | 1271-1272 | Use plain objects and functions |
| `this` keyword | 1245-1285 | Pass context explicitly as a parameter |
| `super` keyword | 1245-1285 | Use explicit function calls |
| `new` operator | 1263-1265 | Use factory functions |

### Variable Declarations

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `var` keyword (statement context) | 210-211 | Use `let` or `const` |
| `var` keyword (for-loop context) | 838-839 | Use `let` or `const` |

### Equality Operators

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `==` (loose equality) | 1324-1330 | Use `===` for strict equality |
| `!=` (loose inequality) | 1324-1330 | Use `!==` for strict inequality |

### Unary Increment/Decrement

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `++x` (prefix increment) | 1299-1304 | Use `x = x + 1` |
| `--x` (prefix decrement) | 1299-1304 | Use `x = x - 1` |

### Postfix Increment/Decrement

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `x++` (postfix increment) | 1566-1572 | Use `x = x + 1` |
| `x--` (postfix decrement) | 1566-1572 | Use `x = x - 1` |

### Compound Assignment Operators (15 total)

All compound assignments follow the pattern: "use `x = x [op] value`"

| Feature | Line(s) | Binary Equivalent |
|---------|---------|-------------------|
| `+=` | 1388-1446 | `x = x + value` |
| `-=` | 1388-1446 | `x = x - value` |
| `*=` | 1388-1446 | `x = x * value` |
| `/=` | 1388-1446 | `x = x / value` |
| `%=` | 1388-1446 | `x = x % value` |
| `**=` | 1388-1446 | `x = x ** value` |
| `&=` | 1388-1446 | `x = x & value` |
| `|=` | 1388-1446 | `x = x | value` |
| `^=` | 1388-1446 | `x = x ^ value` |
| `<<=` | 1388-1446 | `x = x << value` |
| `>>=` | 1388-1446 | `x = x >> value` |
| `>>>=` | 1388-1446 | `x = x >>> value` |
| `&&=` | 1388-1446 | `x = x && value` |
| `||=` | 1388-1446 | `x = x || value` |
| `??=` | 1388-1446 | `x = x ?? value` |

### Type-checking Operator

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `instanceof` | 1366-1369 | Use discriminated unions with tag property |

### Expression-level Features

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| Regular expressions `/.../ ` | 1245-1285 | Use string methods |
| Function expressions (named & anonymous) | 1267-1269 | Use arrow functions `(x) => x * 2` or function declarations |
| `yield` expressions | 1276-1278 | Generators are not available |
| `delete` operator | 1283-1285 | Use object spread to omit properties |

### Global Identifiers

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `Promise` (as unbound global) | 1764-1771 | Use Result types or callbacks |
| `RegExp` (as unbound global) | 1764-1771 | Use string methods |

### Object Built-in Methods

| Feature | Line(s) | Suggested Alternative |
|---------|---------|----------------------|
| `Object.assign()` | 1478-1488 | Use object spread `{...obj1, ...obj2}` |
| `Object.freeze()` | 1478-1488 | Objects are mutable by design |
| `Object.isFrozen()` | 1478-1488 | Objects are mutable by design |

## Error Message Pattern

All error messages follow a consistent format:

```
"'<feature>' is not supported; use <alternative> instead"
```

Examples:
- `'class' is not supported; use plain objects and functions instead`
- `'while' loops are not supported; use 'for-of' with a finite collection instead`
- `'throw' is not supported; use Result types for error handling instead`

## Adding New Unsupported Features

When adding detection for a new unsupported feature:

1. **Determine Layer**:
   - TypeScript-only syntax (no JS equivalent) → Stripper
   - JavaScript feature → Parser

2. **Add Detection Code**:
   - Follow existing error reporting pattern for that layer
   - Include helpful alternative in error message
   - Preserve source location for accurate error reporting

3. **Add Tests**:
   - Stripper: Add test in `zts/stripper.zig` test section
   - Parser: Add test in `zts/parser/test_unsupported.zig`
   - Verify error message content, not just error type

4. **Update This Document**:
   - Add row to appropriate table
   - Include line number, error type, and suggested alternative
   - Update count if adding to parser features

5. **Run Tests**:
   - `zig build test` should pass
   - Manually verify error message quality with test input

## Design Rationale

### Why Two Layers?

**TypeScript Stripper**: Handles TypeScript-specific syntax that has no JavaScript equivalent. Running before the parser allows clean separation - the parser can focus on JavaScript semantics without TS knowledge.

**Parser**: Handles all JavaScript syntax validation. Running after stripping ensures consistent error reporting for both .ts and .js files.

### Why Not Runtime Detection?

Fail-fast at parse time provides:
- Immediate feedback to developers
- Prevents invalid bytecode generation
- Clearer error messages with source context
- No runtime performance overhead

Runtime checks should only exist as defensive programming (e.g., `UnimplementedOpcode`), not primary feature detection.

### Why Duplicate Checks Were Problematic

Before consolidation, `class` was detected in both stripper and parser:
- .ts files: Stripper caught it first with generic error
- .js files: Parser caught it with helpful error message

This created inconsistent developer experience based on file extension. Moving `class` detection entirely to the parser ensures all developers see the same helpful error regardless of file type.

## Migration History

- **Stage 3 (pending)**: Moved `class` and `abstract class` from stripper to parser for consistent error messages
- **Stage 2 (pending)**: Enhanced stripper error logging to include helpful alternatives
- **Initial state**: 48 parser features, 7 stripper features (with class duplication)
