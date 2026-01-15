# TypeScript/TSX Type Stripper — Specification

This document defines a lightweight **TypeScript/TSX type‑stripper** for zts/zigttp. The goal is to erase a useful, meaningful subset of TypeScript syntax **without** adding a full TS compiler, while preserving line/column positions for error reporting.

Status: **Implemented** (see `zts/stripper.zig`).

---

## Goals

- Allow authoring handler code in **TypeScript** and **TSX** by removing type‑only syntax.
- Support **ADT‑style types**, **interfaces**, and **basic generics** on declarations.
- Preserve **runtime JavaScript semantics** and keep output parseable by the existing zts parser.
- Preserve line/column numbers by replacing stripped spans with spaces while keeping newlines.

## Non‑Goals (Explicitly Unsupported)

- `enum` / `const enum`
- `namespace` / `module`
- decorators
- `abstract`, `implements`, parameter properties, access modifiers on class fields
- overload merging or declaration merging semantics
- TSX angle‑bracket assertions (`<T>expr`)
- full TS type system or inference

If unsupported syntax is encountered, the stripper must emit a **clear error** and stop.

---

## Supported Subset

### Type Declarations (stripped entirely)

- `type` aliases (including ADT unions)
- `interface` declarations
- `export type …` / `import type …` (build‑time only)

### Type Annotations (stripped in place)

- Variable annotations: `let x: T = …`
- Parameter annotations: `function f(x: T) { … }`
- Return annotations: `function f(): T { … }`
- Property signatures in type/interface declarations

### Assertions (stripped)

- `as` assertions: `value as T`
- `satisfies` assertions: `value satisfies T`

### Basic Generics (stripped)

- Generic params on type/interface declarations: `type Box<T> = …`
- Generic params on function declarations and arrow functions:
  - `function id<T>(x: T): T { … }`
  - `const id = <T>(x: T): T => x;` (only allowed in **.ts**; in **.tsx** it’s ambiguous and must error)

---

## TSX Handling

- TSX is supported; JSX must remain intact.
- Type annotations inside `{ … }` expressions are stripped normally.
- **Angle‑bracket type assertions** (`<T>expr`) are **disallowed in TSX** to avoid JSX ambiguity.

---

## High‑Level Algorithm

### 1) Tokenization / State Machine

Implement a lightweight scanner with the following states:

- **code**
- **string** (single/double)
- **template** (with interpolation nesting)
- **comment** (line/block)
- **regex** (if your existing tokenizer depends on this)

In **TSX mode**, add JSX‑aware scanning so that:
- JSX tags, attributes, and text are preserved verbatim
- Expressions inside `{ … }` are scanned in **code** state

### 2) Type‑Strip Triggers

Stripper activates on these triggers:

- `type` and `interface` declarations → remove entire declaration
- `import type` / `export type` → remove entire statement (build‑time only)
- `:` annotations on vars/params/returns/properties
- `as` / `satisfies` assertions
- generic parameter lists `<…>` following:
  - `type` name, `interface` name
  - function name or inline function keyword

### 3) Skip Rules / Delimiters

For each trigger, skip text while balancing `()[]{}` and generics `< >` (where appropriate). Stop at a safe delimiter:

- **Annotations**: stop at `, ) ; = => {` (first that closes current context)
- **Return types**: stop at `=>` (arrow) or `{` (function body)
- **`as` / `satisfies`**: stop at `, ) ; }` with balanced nesting
- **Type/interface declarations**: skip entire block `{ … }` and optional trailing `;`

### 4) Preservation of Positions

Replace stripped characters with spaces **without** removing newlines. This preserves line/column numbering for error reporting and stack traces.

---

## Build‑Time Integration

- Add a prepass for `.ts` and `.tsx` sources **before** zts parsing.
- `import type` / `export type` are removed in the prepass (build‑time only).
- Runtime parser receives JS‑only output.

---

## Error Handling & Diagnostics

On unsupported constructs, emit a clear error and abort stripping:

- `enum` / `const enum`
- `namespace`
- decorators (`@something`)
- angle‑bracket assertions in TSX
- class features not supported by zts (if encountered in TS input)

Errors should include the **original line/column** of the problematic construct.

---

## Test Matrix

### A) Type Declarations (TS)

- **Type alias (ADT union)**
  - Input: `type Result<T> = { ok: true, value: T } | { ok: false, error: string };`
  - Expected: declaration stripped

- **Interface**
  - Input: `interface Port<T> { send(msg: T): void; close(): void }`
  - Expected: declaration stripped

- **Type + usage**
  - Input:
    ```ts
    type User = { id: number, name: string };
    let u: User = { id: 1, name: "a" };
    ```
  - Expected:
    ```js
    let u = { id: 1, name: "a" };
    ```

### B) Annotations

- **Variable**
  - Input: `let x: number = 1;`
  - Expected: `let x = 1;`

- **Params + return**
  - Input: `function add(a: number, b: number): number { return a + b; }`
  - Expected: `function add(a, b) { return a + b; }`

- **Arrow**
  - Input: `const f = (x: string): string => x.trim();`
  - Expected: `const f = (x) => x.trim();`

- **Method**
  - Input: `let o = { f(x: number): string { return "" + x; } };`
  - Expected: `let o = { f(x) { return "" + x; } };`

### C) Generics

- **Generic function**
  - Input: `function id<T>(x: T): T { return x; }`
  - Expected: `function id(x) { return x; }`

- **Generic arrow (.ts)**
  - Input: `const id = <T>(x: T): T => x;`
  - Expected: `const id = (x) => x;`

- **Generic type alias**
  - Input: `type Box<T> = { value: T };`
  - Expected: stripped

### D) `as` / `satisfies`

- **as assertion**
  - Input: `let n = (foo as number) + 1;`
  - Expected: `let n = (foo) + 1;`

- **satisfies**
  - Input: `const cfg = { port: 8080 } satisfies Config;`
  - Expected: `const cfg = { port: 8080 };`

### E) `import type` / `export type`

- **import type**
  - Input: `import type { Foo } from "./types";`
  - Expected: removed entirely

- **export type**
  - Input: `export type { Foo };`
  - Expected: removed entirely

### F) TSX‑Specific

- **JSX preserved**
  - Input: `const el = <div className="x">{count: number}</div>;`
  - Expected: `const el = <div className="x">{count}</div>;`

- **TSX + return type**
  - Input: `function View(): JSX.Element { return <div/>; }`
  - Expected: `function View() { return <div/>; }`

### G) Negative / Unsupported

- **enum**
  - Input: `enum Color { Red, Blue }`
  - Expected: error “enum not supported”

- **namespace**
  - Input: `namespace N { export const x = 1 }`
  - Expected: error “namespace not supported”

- **decorator**
  - Input: `@sealed class X {}`
  - Expected: error “decorators not supported”

- **TSX angle‑bracket assertion**
  - Input: `const x = <Foo>bar;`
  - Expected: error “angle‑bracket assertions not supported in TSX”

### H) Line/Column Preservation

- **Mapping sanity**
  - Input:
    ```ts
    type X = { a: number };
    let x: X = 1;
    console.log(x);
    ```
  - Expected: `console.log` line number unchanged

---

## Integration Notes (Future)

- Add a `typestrip` prepass in `zts/parser/root.zig` for `.ts` and `.tsx` files.
- Wire it into `src/zruntime.zig` load path, before zts parsing.
- Keep the prepass optional and off for `.js` / `.jsx` sources.

