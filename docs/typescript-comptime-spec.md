# TypeScript `comptime` (Strict) — Specification (Option A)

This document specifies a **strict compile‑time evaluation** feature for zts by **extending the existing TypeScript/TSX stripper** (`zts/stripper.zig`). The goal is to evaluate explicitly marked expressions at compile time and replace them with literal values, while keeping the existing type‑stripping behavior intact.

Status: **spec only** (no implementation yet).

---

## Overview

- **Syntax:** `comptime(<expr>)`
- **Strict mode:** any unsupported operation or runtime dependency is a compile‑time error.
- **Scope:** implemented as a pre‑parse transformation in `zts/stripper.zig`.
- **Compatibility:** works for `.ts` and `.tsx` sources; JSX preserved in TSX.

---

## Goals

- Allow **compile‑time constant evaluation** in TS/TSX code.
- Preserve **line/column positions** for diagnostics by blanking stripped spans.
- Reuse the existing stripper pipeline without altering the core JS parser.

## Non‑Goals

- Full TypeScript evaluation or type‑system inference
- Executing arbitrary JS at compile time
- Runtime dependency injection into comptime

---

## Syntax

**Required syntax (only supported form):**

```ts
const value = comptime(1 + 2 * 3);
```

Rationale:
- Valid JS syntax even without stripping.
- No JSX ambiguity.
- Simple to detect in the stripper.

---

## Strict Evaluation Rules

### Allowed

**Literals**
- number, string, boolean, `null`, `undefined`, `NaN`, `Infinity`

**Unary**
- `+ - ! ~`

**Binary**
- `+ - * / % **`
- `| & ^ << >> >>>`
- `== != === !== < <= > >=`
- `&& || ??`

**Ternary**
- `cond ? a : b` (all sub‑expressions comptime)

**Grouping**
- Parentheses

**Arrays/Objects**
- `[1, 2, 3]`, `{ a: 1, b: "x" }` with comptime values only

**Whitelisted globals** (pure subset)
- `Math` constants: `Math.PI`, `Math.E`, `Math.SQRT2`, etc.
- `Math` pure functions: `abs`, `min`, `max`, `floor`, `trunc`, `clz32`, `imul`, `fround`, etc.

> The whitelist must be explicit and minimal.

### Disallowed (Strict Errors)

- Any variable not explicitly whitelisted
- Any function call except whitelisted `Math.*`
- Property access except `Math.*` constants
- `new`, `this`, `super`, `arguments`, `eval`
- `Date.now`, `Math.random`
- Assignments, update expressions, `delete`, `throw`, `try`, loops
- Function literals, class literals, or closures

---

## Error Handling (Strict)

On any unsupported construct or failure, **stop stripping and return an error**.

Suggested error categories:
- `ComptimeUnsupportedOp`
- `ComptimeUnknownIdentifier`
- `ComptimeCallNotAllowed`
- `ComptimeSyntaxError`
- `ComptimeDepthExceeded`

Error messages should include **line/column** in the original file.

---

## Implementation (Option A — Stripper Extension)

### Integration Points

- Extend `StripOptions` with:
  - `enable_comptime: bool`
  - `comptime_strict: bool = true`
- In `processToken()` (stripper):
  - Detect identifier `comptime` followed by `(`
  - Parse balanced parentheses to extract expression span
  - Evaluate expression (see evaluator spec)
  - Replace entire span with a **literal string**

### Preserve Line/Column

Introduce `emitReplacedSpan(start, end, replacement)`:
- Emit replacement text (single‑line literal)
- For the rest of the span:
  - Preserve newlines
  - Replace other chars with spaces

This keeps error positions consistent with the original source.

---

## Expression Evaluator

Implement a **minimal expression parser** inside the stripper:

- Pratt or recursive‑descent
- Only expression grammar (no statements)
- JS‑like precedence for allowed operators

### Output Type

`ComptimeValue` union:
- `number`, `string`, `bool`, `null`, `undefined`, `array`, `object`

### Literal Emission

- Numbers: decimal
- Strings: escaped with quotes
- Objects: `{a:1}`
- Arrays: `[1,2,3]`
- Wrap objects in `({ … })` when replacing in expression context to avoid parsing ambiguities.

---

## TSX Behavior

- JSX structure is preserved.
- `comptime(...)` inside `{ ... }` works normally.
- Angle‑bracket type assertions remain unsupported in TSX.

---

## Performance Guards

Add hard limits to prevent pathological compile‑time cost:

- Max comptime expression length (e.g., 4–8 KB)
- Max AST depth
- Max literal size emitted

---

## Test Matrix (Additive)

### Positive

```ts
const x = comptime(1 + 2 * 3);          // -> const x = 7;
const s = comptime("a" + "b");        // -> const s = "ab";
const v = comptime(Math.max(1, 2));     // -> const v = 2;
const o = comptime({ a: 1, b: 2 });     // -> const o = { a: 1, b: 2 };
```

TSX:
```tsx
const el = <div>{comptime(1+2)}</div>;  // -> <div>{3}</div>
```

### Negative (Strict Errors)

```ts
comptime(Date.now())      // ComptimeCallNotAllowed
comptime(Math.random())   // ComptimeCallNotAllowed
comptime(foo + 1)         // ComptimeUnknownIdentifier
comptime(() => 1)         // ComptimeUnsupportedOp
```

---

## Integration Summary

- **Stripper**: extend to parse and replace `comptime(...)`.
- **Runtime**: enable for `.ts` / `.tsx` inputs (configurable).
- **Parser**: unchanged.

---

## Future Extensions (Optional)

- Allow user‑defined `const` values in a `comptime env` map
- Cache evaluated expressions to speed repeated compilation
- Add `comptime` to bytecode constant pool if upstream compiler gains constant folding

