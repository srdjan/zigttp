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

### 6. typeof guard type narrowing

When an `if` condition is a `typeof` guard, the BoolChecker narrows the variable's type within the appropriate branch. This catches more bugs statically while remaining sound (no false positives).

```javascript
function handler(req) {
    const val = req.headers.get("x-flag");

    if (typeof val === "number") {
        if (val) { /* ... */ }  // ERROR: number in boolean context
    }

    if (typeof val === "boolean") {
        if (val) { /* ... */ }  // OK: val is boolean here
    }

    // Negated guards narrow the else-branch
    if (typeof val !== "number") {
        // val is still unknown here
    } else {
        if (val) { /* ... */ }  // ERROR: number in boolean context
    }

    // Compound guards with && narrow all variables
    if (typeof x === "boolean" && typeof y === "boolean") {
        const r = x && y;      // OK: both are boolean
    }

    return Response.json({ ok: true });
}
```

Narrowing is branch-scoped: it does not leak outside the guarded branch. Nested typeof guards compose correctly. Reassignment inside a guarded branch invalidates the narrowing.

Only `typeof x === "T"` and `"T" === typeof x` patterns are recognized. Other forms like `x === true` do not trigger narrowing.

## The `unknown` Escape Hatch

The BoolChecker performs lightweight type inference by walking the IR tree. When it cannot determine the type of an expression statically, it infers `unknown`. This happens for:

- Function parameters (narrowed inside typeof guard branches)
- Function call results (unless the callee is a tracked local function or a modeled virtual-module import)
- Property accesses (except modeled Result properties such as `result.ok`)
- Computed accesses (e.g., `arr[i]`)

When `unknown` appears in a boolean context, no diagnostic is emitted. The static checker only rejects code it can prove is non-boolean. Runtime VM assertions catch the remaining cases when the code actually executes.

## Progressive Inference

Recent sound-mode passes extend the lightweight inference beyond literals and local bindings:

### Known virtual-module imports

Imported functions from `zigttp:*` modules can carry known return types into the checker.

```javascript
import { verifyWebhookSignature } from "zigttp:auth";
import { cacheIncr } from "zigttp:cache";
import { env } from "zigttp:env";

if (verifyWebhookSignature(payload, secret, signature)) {
    // OK: boolean
}

if (cacheIncr("stats", "requests")) {
    // ERROR: number in boolean context
}

const apiKey = env("API_KEY");
if (apiKey !== undefined) {
    // OK: explicit check for env()'s maybe-missing return
}
```

### Nullable returns and `??`

The checker models "value may be missing" returns from functions such as `env()`, `parseBearer()`, `cacheGet()`, and `routerMatch()` as nullable variants. These are rejected in boolean contexts until you narrow them explicitly.

```javascript
const cached = cacheGet("api", req.url);
if (cached !== null) {
    return Response.json(JSON.parse(cached));
}

const appName = env("APP_NAME") ?? "zigttp";
// Inferred as string
```

`??` also emits a warning when the left side is provably never null or undefined.

### Result-shaped property access

Values returned from `jwtVerify()`, `validateJson()`, `validateObject()`, and `coerceJson()` keep enough shape information for common Result properties.

```javascript
const result = jwtVerify(token, secret);

if (result.ok) {
    // OK: result.ok is inferred boolean
    return Response.json(result.value);
}

if (result.error) {
    // ERROR: string in boolean context
}
```

### Match expressions

When all `match` arms resolve to the same type, the whole expression inherits that type.

```javascript
const count = match (kind) {
    when "fast": 1,
    when "slow": 2,
    default: 0
};

if (count) {
    // ERROR: number in boolean context
}
```

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
| `let x = expr; ... x` | same as expr (invalidated on reassignment) |
| `cond ? a : b` | unified type of a and b |
| `match (...) { ... }` | unified arm type (if compatible) |
| `const f = (x) => x > 0; f(1)` | return type of f (boolean) |
| `const f = (x) => { return x * 2; }; f(1)` | return type of f (number) |
| imported virtual-module call | known return type when modeled |
| nullable virtual-module return | nullable string/object |
| function calls (untracked callee) | unknown |
| Result property access (`result.ok`) | known property type when modeled |
| property access (general case) | unknown |
| `typeof x === "T"` guard (then-branch) | T (narrowed) |
| `typeof x !== "T"` guard (else-branch) | T (narrowed) |

## Migration Guide

| Before (standard JS) | After (sound mode) |
|---|---|
| `if (x)` | `if (x !== undefined && x !== null)` |
| `if (env("KEY"))` | `if (env("KEY") !== undefined)` |
| `if (cacheGet(ns, key))` | `if (cacheGet(ns, key) !== null)` |
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
- `non-boolean value (string?) used in boolean context` - help: use an explicit null/undefined check before treating the value as present
- `non-boolean value (object?) used in boolean context` - help: use an explicit null/undefined check before using the object

**Warnings (do not block compilation):**
- `left side of '??' is never null or undefined` - help: remove the '??' fallback; it is unreachable

## Runtime Assertions

When sound mode is enabled, the VM enforces boolean values at three opcode sites:

- `if_true` / `if_false` / `if_false_goto`: conditional jumps assert `isBool()` before branching
- `not`: logical NOT asserts `isBool()` before negating

If a non-boolean value reaches these opcodes at runtime, an exception is set: `sound mode: condition must be boolean, got <type>`.

Performance impact is negligible: `isBool()` is two u64 comparisons against constants, and the branch predictor nearly always takes the non-error path.

**JIT suppression**: Sound mode disables JIT tier promotion. The JIT compiles conditional jumps to native branches without `isBool()` guards, so all execution stays in the interpreter where assertions are enforced. For FaaS workloads this is typically not a concern since most handlers run below the JIT threshold anyway.
