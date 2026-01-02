# TypeScript `comptime()` Feature - Specification

This document specifies the **compile-time evaluation** feature for zts, implemented as an extension to the TypeScript/TSX stripper (`zts/stripper.zig`). The feature evaluates explicitly marked expressions at compile time and replaces them with literal values.

Status: **Implemented** in `zts/comptime.zig` and integrated with `zts/stripper.zig`.

---

## Overview

- **Syntax:** `comptime(<expr>)`
- **Strict mode:** any unsupported operation or runtime dependency is a compile-time error.
- **Scope:** implemented as a pre-parse transformation in `zts/stripper.zig`.
- **Compatibility:** works for `.ts` and `.tsx` sources; JSX preserved in TSX.

---

## Usage Examples

```typescript
// Basic arithmetic
const x = comptime(1 + 2 * 3);              // -> const x = 7;

// String operations
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";
const parts = comptime("a,b,c".split(","));    // -> const parts = ["a","b","c"];

// Math functions
const pi = comptime(Math.PI);               // -> const pi = 3.141592653589793;
const max = comptime(Math.max(1, 5, 3));    // -> const max = 5;

// Objects and arrays
const cfg = comptime({ timeout: 30 });      // -> const cfg = ({timeout:30});
const arr = comptime([1, 2, 3]);            // -> const arr = [1,2,3];

// Hash function (FNV-1a)
const etag = comptime(hash("content-v1")); // -> const etag = "a1b2c3d4";

// JSON parsing
const config = comptime(JSON.parse('{"a":1}')); // -> const config = ({a:1});

// Environment variables (when configured)
const region = comptime(Env.AWS_REGION);    // -> const region = "us-east-1";

// Build metadata (when configured)
const version = comptime(__VERSION__);      // -> const version = "1.0.0";

// TSX
const el = <div>{comptime(1+2)}</div>;      // -> <div>{3}</div>
```

---

## Supported Operations

### Literals
- number, string, boolean, `null`, `undefined`, `NaN`, `Infinity`

### Unary Operators
- `+ - ! ~`

### Binary Operators
- Arithmetic: `+ - * / % **`
- Bitwise: `| & ^ << >> >>>`
- Comparison: `== != === !== < <= > >=`
- Logical: `&& || ??`

### Ternary Operator
- `cond ? a : b` (all sub-expressions comptime)

### Arrays and Objects
- `[1, 2, 3]`, `{ a: 1, b: "x" }` with comptime values only

### Math Constants
- `Math.PI`, `Math.E`, `Math.LN2`, `Math.LN10`, `Math.LOG2E`, `Math.LOG10E`, `Math.SQRT2`, `Math.SQRT1_2`

### Math Functions
- `abs`, `floor`, `ceil`, `round`, `trunc`, `sqrt`, `cbrt`
- `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`
- `log`, `log2`, `log10`, `exp`, `pow`
- `min`, `max`, `sign`, `clz32`, `imul`, `fround`, `hypot`

### String Properties
- `length`

### String Methods
- `toUpperCase()`, `toLowerCase()`
- `trim()`, `trimStart()`, `trimEnd()` (aliases: `trimLeft()`, `trimRight()`)
- `slice(start, end?)`, `substring(start, end?)`
- `includes(search)`, `startsWith(search)`, `endsWith(search)`, `indexOf(search)`
- `charAt(index)`
- `split(delimiter)`
- `repeat(count)`
- `replace(search, replacement)`, `replaceAll(search, replacement)`
- `padStart(length, padStr?)`, `padEnd(length, padStr?)`

### Array Properties
- `length`

### Built-in Functions
- `parseInt(str, radix?)`, `parseFloat(str)`
- `JSON.parse(str)` - parses JSON string to comptime value
- `hash(str)` - FNV-1a hash, returns 8-char hex string

### Environment Variables (Optional)
- `Env.VARNAME` - access environment variables at compile time
- Configured via `StripOptions.comptime_env`

### Build Metadata (Optional)
- `__BUILD_TIME__` - ISO timestamp of build
- `__GIT_COMMIT__` - git commit hash
- `__VERSION__` - version string
- Configured via `StripOptions.comptime_env`

---

## Disallowed Operations (Compile-Time Errors)

- Any variable not explicitly whitelisted
- Any function call except whitelisted ones
- `Date.now()`, `Math.random()` (non-deterministic)
- `new`, `this`, `super`, `arguments`, `eval`
- Assignments, update expressions, `delete`, `throw`, `try`, loops
- Function literals, class literals, or closures

---

## Error Types

| Error | Description |
|-------|-------------|
| `ComptimeUnsupportedOp` | Operation not supported in comptime context |
| `ComptimeUnknownIdentifier` | Variable/function not whitelisted |
| `ComptimeCallNotAllowed` | Function call not allowed (e.g., `Math.random()`) |
| `ComptimeSyntaxError` | Syntax error in comptime expression |
| `ComptimeDepthExceeded` | Expression nesting too deep (max 64) |
| `ComptimeExpressionTooLong` | Expression exceeds 8KB limit |
| `ComptimeTypeMismatch` | Type error (e.g., string op on number) |
| `ComptimeDivisionByZero` | Division by zero |
| `ComptimeUnclosedString` | Unterminated string literal |
| `ComptimeUnclosedParen` | Unbalanced parentheses |
| `ComptimeOutOfMemory` | Memory allocation failed |

Error messages include line/column information from the original source.

---

## Implementation Details

### Files

- **`zts/comptime.zig`** - Core evaluator (~2000 lines)
  - `ComptimeValue` union: number, string, boolean, null, undefined, nan, infinity, array, object
  - `ComptimeEvaluator` struct with Pratt parser
  - `emitLiteral()` - serialize values back to JS source
  - Math, string, hash implementations
  - Memory management with proper cleanup

- **`zts/stripper.zig`** - Integration
  - Extended `StripOptions` with `enable_comptime` and `comptime_env`
  - `tryEvaluateComptime()` - detect and evaluate `comptime(...)` expressions

### Stripper Integration

```zig
pub const StripOptions = struct {
    tsx_mode: bool = false,
    enable_comptime: bool = false,
    comptime_env: ?*ComptimeEnv = null,
};

pub const ComptimeEnv = struct {
    env_vars: ?std.StringHashMap([]const u8) = null,
    build_time: ?[]const u8 = null,
    git_commit: ?[]const u8 = null,
    version: ?[]const u8 = null,
};
```

### Literal Emission

| Type | Output |
|------|--------|
| Integer | `42` |
| Float | `3.14` |
| String | `"hello"` (with escaping) |
| Boolean | `true` / `false` |
| Null | `null` |
| Array | `[1,2,3]` |
| Object | `({a:1,b:2})` (wrapped for expression safety) |
| NaN | `NaN` |
| Infinity | `Infinity` / `-Infinity` |

### Performance Guards

- Max expression length: 8KB
- Max AST depth: 64
- Memory managed with proper allocator cleanup

---

## Test Coverage

### Positive Tests
- Basic arithmetic and string operations
- Math constants and functions (all listed above)
- String methods (all listed above)
- Method chaining: `"  hello  ".trim().toUpperCase()`
- Arrays and objects
- Ternary and logical operators
- Hash and JSON.parse functions
- Environment variables and build metadata

### Negative Tests
- `comptime(Date.now())` - ComptimeCallNotAllowed
- `comptime(Math.random())` - ComptimeCallNotAllowed
- `comptime(foo + 1)` - ComptimeUnknownIdentifier
- `comptime(() => 1)` - ComptimeUnsupportedOp

### TSX Tests
- `<div>{comptime(1+2)}</div>` - transforms to `<div>{3}</div>`

---

## Future Extensions (Optional)

- Cache evaluated expressions to speed repeated compilation
- Add `comptime` to bytecode constant pool if upstream compiler gains constant folding
- Additional string/array methods as needed
