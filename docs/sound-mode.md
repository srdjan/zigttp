# Type-Directed Truthiness

zigttp uses type-directed truthiness (TDT) in boolean contexts. Instead of rejecting all non-boolean values, the compiler uses its type knowledge to apply unambiguous truthiness rules per type. The original concise syntax (`if (x)`, `if (count)`, `if (name)`) works when the type has exactly one falsy state.

Objects and functions remain rejected because they are always truthy - a condition on them is pointless and likely a bug.

## Truthiness Rules

| Type | Boolean meaning | Rationale |
|---|---|---|
| `boolean` | the value itself | already boolean |
| `number` | `!= 0` | exactly one falsy state (zero) |
| `string` | `!= ""` | exactly one falsy state (empty) |
| `undefined` | always false | no ambiguity; WARNING as dead branch |
| `optional_string` | `!= undefined` | narrows to `string` in then-branch |
| `optional_object` | `!= undefined` | narrows to `object` in then-branch |
| `object` | REJECTED | always truthy - condition is pointless |
| `function` | REJECTED | always truthy - condition is pointless |
| `unknown` | runtime coercion | same rules at runtime; objects/functions still error |

## What Works

### Optional existence checks (most common)

Every `env()`, `cacheGet()`, `parseBearer()`, `routerMatch()` call returns an optional type. Use them directly in conditions:

```javascript
import { env } from "zigttp:env";
import { parseBearer } from "zigttp:auth";
import { routerMatch } from "zigttp:router";

const token = parseBearer(auth);
if (token) { use(token); }        // token narrowed to string inside

const match = routerMatch(routes, req);
if (match) {                       // match narrowed to object inside
    req.params = match.params;
    return match.handler(req);
}

const secret = env("SECRET");
if (!secret) { return Response.json({ error: "missing secret" }, { status: 500 }); }
// secret is string here (narrowed by early return)
```

### Number and string truthiness

```javascript
if (count) { ... }                 // means count !== 0
if (name) { ... }                  // means name !== ""
if (!count) { ... }                // means count === 0
```

### Logical operators

`&&` and `||` operands are auto-coerced by the conditional opcodes. The return value is still the operand value (JS semantics), not boolean.

```javascript
if (count && name) { ... }         // both coerced: count != 0 && name != ""
const val = x ?? fallback;         // use ?? for value defaults (unchanged)
```

### Negation with narrowing

```javascript
const token = parseBearer(auth);
if (!token) { return error; }      // token narrowed to undefined inside
// token is string here (after early return)
```

## What Is Rejected

### Objects (always truthy)

```javascript
if ({}) { ... }                    // ERROR: always-truthy value (object)
if (result) { ... }                // ERROR if result is known object type

// Use result.ok instead:
if (result.ok) { ... }             // OK: result.ok is boolean
```

### Functions (always truthy)

```javascript
if (() => 1) { ... }               // ERROR: always-truthy value (function)
```

### Undefined (warning)

```javascript
if (undefined) { ... }             // WARNING: condition is always false (dead branch)
```

## Narrowing

When `if (x)` and x has an optional type (`optional_string` or `optional_object`), x is narrowed to its non-optional variant in the then-branch. This works automatically - no explicit `x !== undefined` needed.

When `if (!x)` and x is optional, x is narrowed to `undefined` in the then-branch. The negated narrowing also applies to else-branches and early returns.

The existing `typeof` guard narrowing and `x !== undefined` narrowing continue to work unchanged. TDT narrowing is applied when no explicit guard pattern is detected.

```javascript
import { env } from "zigttp:env";

const val = env("KEY");            // optional_string
if (val) {
    // val is string here
    const upper = val;
}
// val reverts to optional_string here
```

## Nullish coalescing ?? warnings

When the left side of `??` is provably non-nullable, a warning is emitted:

```javascript
42 ?? 0                            // WARNING: LHS is never undefined
env("KEY") ?? "default"            // No warning: env() is optional
```

## == and != are globally banned

The parser rejects `==` and `!=` with a helpful error message suggesting `===` and `!==`.

## The `unknown` Escape Hatch

When the BoolChecker cannot determine the type statically, it infers `unknown`. This happens for function parameters, untracked function calls, and general property accesses. `unknown` values pass the static checker silently. The VM applies the same TDT rules at runtime: number/string/boolean/undefined are coerced, objects/functions produce an error.

## Type Inference Rules

| Expression | Inferred Type |
|---|---|
| `true`, `false` | boolean |
| `42`, `3.14` | number |
| `"hello"`, `` `template` `` | string |
| `undefined` | undefined |
| `{}`, `[]` | object |
| `() => ...`, `function() {}` | function |
| `===`, `!==`, `<`, `>`, `<=`, `>=`, `in` | boolean |
| `&&`, `\|\|` | boolean |
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
| imported virtual-module call | known return type when modeled |
| optional virtual-module return | optional string/object |
| Result property access (`result.ok`) | known property type when modeled |
| `if (x)` where x is optional | narrows to non-optional in then-branch |
| `typeof x === "T"` guard (then-branch) | T (narrowed) |
| `typeof x !== "T"` guard (else-branch) | T (narrowed) |

## Diagnostic Reference

**Errors (block compilation):**
- `always-truthy value (object) in 'if' operator` - objects are always truthy; this condition is pointless
- `always-truthy value (function) in 'if' operator` - functions are always truthy; this condition is pointless

**Warnings (do not block compilation):**
- `condition is always false (undefined)` - this branch is dead code
- `left side of '??' is never undefined` - remove the '??' fallback; it is unreachable

## Runtime Enforcement

The VM applies TDT at four opcode sites:

- `if_true` / `if_false` / `if_false_goto`: conditional jumps use `toConditionBool()` - accepts boolean/number/string/undefined, rejects objects/functions
- `not`: logical NOT uses `toConditionBool()` with the same rules

If an object or function reaches these opcodes at runtime, an exception is set: `condition rejected: <type> has no falsy state`.

The boolean fast path remains first in the check sequence. The JIT includes an additional integer fast path for conditionals and NOT, avoiding deoptimization for the two most common types (boolean and integer).
