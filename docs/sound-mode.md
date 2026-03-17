# Sound Mode: Strict Boolean Enforcement

Sound mode adds compile-time and runtime enforcement that boolean contexts require actual boolean values. JavaScript's truthy/falsy coercion is a major source of bugs: `if (count)` silently passes when count is 0, `user && user.name` returns the user object instead of a boolean, `!""` is `true`. Sound mode catches these before deployment.

## Enabling Sound Mode

**CLI (runtime checking):**
```bash
zigttp-server --sound -e "function handler(req) { ... }"
zigttp-server --sound examples/handler.jsx
```

**Build option (compile-time checking):**
```bash
zig build -Dhandler=handler.jsx -Dsound
```

When enabled at build time, the BoolChecker runs during precompilation and fails the build on violations. When enabled at runtime via `--sound`, violations in dynamically-loaded handlers are caught before execution, and any values the static checker could not prove (the `unknown` escape hatch) are caught by runtime assertions in the VM.

## What Sound Mode Enforces

### 1. Boolean-only if/ternary conditions

The condition expression in `if` statements and ternary operators must produce a boolean value.

```javascript
// Passes
if (x === 0) { ... }
if (arr.length > 0) { ... }
if (flag) { ... }  // if flag is known boolean or unknown

// Fails
if (0) { ... }              // number in condition
if ("hello") { ... }        // string in condition
if (null) { ... }           // null in condition
const count = 42;
if (count) { ... }          // tracked const number
```

### 2. Boolean-only && and || operands

Both sides of `&&` and `||` must be boolean. This prevents the common JS pattern of using `&&` for conditional execution and `||` for default values (use `??` for defaults).

```javascript
// Passes
if (a > 0 && b !== null) { ... }
const ok = isValid() || isFallback();

// Fails
1 && 2               // number operands
"a" || "b"           // string operands
user && user.name    // object operand (use if instead)
```

### 3. Boolean-only ! (logical NOT)

The operand of `!` must be boolean.

```javascript
// Passes
const notDone = !(x > 0);
const inverted = !flag;  // if flag is boolean or unknown

// Fails
!0            // number operand
!"str"        // string operand
!null         // null operand
```

### 4. Nullish coalescing ?? warnings

When the left side of `??` is provably non-nullable (number, string, boolean, object, or function), a warning is emitted since the fallback is unreachable.

```javascript
// Warning: LHS is never null/undefined
42 ?? 0
"str" ?? "default"

// No warning: unknown types may be nullable
getValue() ?? fallback
param ?? defaultValue
```

### 5. == and != are globally banned

The parser already rejects `==` and `!=` with a helpful error message suggesting `===` and `!==`. This is independent of sound mode and always active.

## The `unknown` Escape Hatch

The BoolChecker performs lightweight type inference by walking the IR tree. When it cannot determine the type of an expression statically, it infers `unknown`. This happens for:

- Function parameters
- Function call results
- Property accesses (e.g., `obj.flag`)
- Let variables (mutable, could be reassigned)
- Computed accesses (e.g., `arr[i]`)

When `unknown` appears in a boolean context, no diagnostic is emitted. The static checker only rejects code it can prove is non-boolean. Runtime VM assertions catch the remaining cases when the code actually executes.

## Type Inference Rules

| Expression | Inferred Type |
|---|---|
| `true`, `false` | boolean |
| `42`, `3.14` | number |
| `"hello"`, `` `template` `` | string |
| `null` | null |
| `undefined` | undefined |
| `{}`, `[]` | object |
| `() => ...`, `function() {}` | function |
| `===`, `!==`, `<`, `>`, `<=`, `>=`, `in` | boolean |
| `&&`, `\|\|` (sound mode) | boolean |
| `+` (string + any) | string |
| `+` (number + number) | number |
| `-`, `*`, `/`, `%`, `**` | number |
| `&`, `\|`, `^`, `<<`, `>>`, `>>>` | number |
| `!` | boolean |
| `-x`, `+x`, `~x` | number |
| `typeof` | string |
| `void` | undefined |
| `const x = expr; ... x` | same as expr |
| `cond ? a : b` | type of a (if a and b match) |
| function calls, property access | unknown |

## Migration Guide

| Before (standard JS) | After (sound mode) |
|---|---|
| `if (x)` | `if (x !== undefined && x !== null)` |
| `if (count)` | `if (count !== 0)` |
| `if (name)` | `if (name.length > 0)` or `if (name !== "")` |
| `x && doSomething()` | `if (x) { doSomething(); }` |
| `x \|\| defaultValue` | `x ?? defaultValue` |
| `!0` | `false` |
| `!!x` | `x !== null && x !== undefined` |

## Diagnostic Reference

All sound mode diagnostics are prefixed with `sound error:` or `sound warning:`.

**Errors (block compilation):**
- `non-boolean value (number) used in boolean context` - help: use explicit comparison: n !== 0
- `non-boolean value (string) used in boolean context` - help: use explicit comparison: s.length > 0 or s !== ""
- `non-boolean value (null) used in boolean context` - help: null is not boolean; this condition is always false
- `non-boolean value (undefined) used in boolean context` - help: undefined is not boolean; this condition is always false
- `non-boolean value (object) used in boolean context` - help: objects are not boolean; this condition is always true
- `non-boolean value (function) used in boolean context` - help: functions are not boolean; this condition is always true

**Warnings (do not block compilation):**
- `left side of '??' is never null or undefined` - help: remove the '??' fallback; it is unreachable

## Runtime Assertions

When sound mode is enabled, the VM enforces boolean values at three opcode sites:

- `if_true` / `if_false` / `if_false_goto`: conditional jumps assert `isBool()` before branching
- `not`: logical NOT asserts `isBool()` before negating

If a non-boolean value reaches these opcodes at runtime, an exception is set: `sound mode: condition must be boolean, got <type>`.

Performance impact is negligible: `isBool()` is two u64 comparisons against constants, and the branch predictor nearly always takes the non-error path.
