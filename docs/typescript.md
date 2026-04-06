# TypeScript Support

zigttp includes native TypeScript and TSX support through two features: a type stripper that removes type annotations at load time, and a compile-time evaluator for the `comptime()` function.

---

## Type Stripper

The type stripper (`zigts/stripper.zig`) removes TypeScript syntax before parsing, preserving line/column positions for error reporting by replacing stripped spans with spaces.

### Supported Subset

**Type declarations** (stripped entirely):
- `type` aliases (including ADT unions)
- `distinct type` declarations (nominal/branded types)
- `interface` declarations
- `export type ...` / `export distinct type ...` / `import type ...`

**Type annotations** (stripped in place):
- Variable annotations: `let x: T = ...`
- Parameter annotations: `function f(x: T) { ... }`
- Return annotations: `function f(): T { ... }`

**Assertions** (stripped):
- `as` assertions: `value as T`
- `satisfies` assertions: `value satisfies T`

**Basic generics** (stripped):
- Generic params on type/interface: `type Box<T> = ...`
- Generic params on functions: `function id<T>(x: T): T { ... }`
- Generic arrow functions in .ts files: `const id = <T>(x: T): T => x;`

**Generic type aliases** (stripped and type-checked):

Generic type aliases like `type Result<T> = { ok: boolean; value: T }` are stripped at load time and resolved by the type checker. When the alias is used in an annotation (`const x: Result<string>`), the type checker instantiates the body by substituting the type parameters with the provided arguments, producing a concrete record type for structural checking.

```typescript
type Result<T> = { ok: boolean; value: T; error: string };
type Pair<A, B> = { first: A; second: B };

const auth: Result<object> = jwtVerify(token, secret);  // checked as { ok: boolean; value: object; error: string }
const pair: Pair<string, number> = { first: "a", second: 1 };
```

Up to 8 type parameters per alias are supported.

### Examples

```typescript
// Input
type User = { id: number; name: string };
let u: User = { id: 1, name: "a" };
function add(a: number, b: number): number { return a + b; }
const x = (foo as number) + 1;

// After stripping
let u         = { id: 1, name: "a" };
function add(a         , b         )          { return a + b; }
const x = (foo          ) + 1;
```

### Unsupported TypeScript Features

These produce clear error messages at strip or parse time:

| Feature | Error Location | Suggested Alternative |
|---------|---------------|----------------------|
| `any` type | Stripper | Use specific types or union types |
| `enum` / `const enum` | Parser | Use object literals or discriminated unions |
| `namespace` / `module` | Parser | Use ES6 modules |
| `implements` | Parser | Use duck typing or runtime checks |
| `@decorator` syntax | Parser | Use function composition |
| Access modifiers (`public`, `private`, `protected`) | Parser | Use naming conventions |

### TSX Handling

TSX is supported: JSX tags remain intact while type annotations inside `{ ... }` expressions are stripped normally. Angle-bracket type assertions (`<T>expr`) are disallowed in TSX to avoid JSX ambiguity.

---

## Type Checking

The type checker (`zigts/type_checker.zig`) validates type annotations at build time. It runs after stripping and parsing, before bytecode generation.

### Checked Properties

- Variable declaration types match initializer types
- Function argument types match declared parameter types
- Return values match declared return types
- Property access on known record types (including `readonly` enforcement)
- Virtual module function signatures (argument count and types)
- Discriminated union narrowing in `match` expressions and `if` conditions
- Nominal type safety for `distinct type` declarations
- Template literal type pattern matching
- Type guard narrowing (`x is T`) in `if` branches and `assert` statements

### Structural Matching

Object literals are structurally matched against declared interface and type alias types. A `{ message: string, count: number }` literal passes as a `ResponseData` interface if the fields match, regardless of whether the type was declared as `type` or `interface`.

Interfaces whose members are all functions are treated as nominal (identity-based matching only). This prevents structural forgery of capability objects.

### Optional Narrowing

The type checker narrows nullable types through if-guards. Functions like `env()`, `cacheGet()`, and `parseBearer()` return optional values (`T | undefined`). Three guard patterns trigger narrowing:

```typescript
const val = env("KEY");

if (val) {
    // val is string here (narrowed from string | undefined)
    sha256(val);
}

if (!val) return Response.text("missing");
// val is string here (early return pattern)

if (val !== undefined) {
    // val is string here (explicit check)
}
```

### Discriminated Union Narrowing

Discriminated unions narrow through `if` conditions on tag fields:

```typescript
type Result = { kind: "ok", value: string } | { kind: "err", error: string };

if (r.kind === "err") {
    return Response.json({ error: r.error }, { status: 400 });
}
// r is narrowed to { kind: "ok", value: string } from here
r.value.toUpperCase();
```

`match` handles exhaustive branching. `if` handles control flow with early returns. Different tools for different jobs.

### Type Guards and Assert

Type guard functions narrow in `if` branches. The `assert` statement installs permanent forward narrowing:

```typescript
function isString(x: unknown): x is string {
    return typeof x === "string";
}

if (isString(val)) {
    val.toUpperCase();   // narrowed in then-branch
}

assert isString(val);
val.toUpperCase();       // narrowed from here forward

assert isString(name), Response.json({ error: "name required" }, { status: 400 });
```

When `assert` fails with no error expression, the handler halts. With an explicit error expression, that value is returned.

### Distinct Types

`distinct type` creates nominal types that prevent accidental cross-assignment:

```typescript
distinct type UserId = string;
distinct type SessionId = string;

const uid: UserId = UserId("usr_123");     // constructor wraps the base type
const sid: SessionId = SessionId("sess");

const lookup = (id: UserId) => id;
lookup(uid);    // OK
lookup(sid);    // ERROR: SessionId is not assignable to UserId
lookup("raw");  // ERROR: string is not assignable to UserId

uid.toUpperCase();  // operations unwrap to base type
```

### Readonly Fields

The `readonly` modifier prevents assignment to record fields:

```typescript
type Config = { readonly port: number; host: string };
const cfg: Config = { port: 3000, host: "localhost" };
cfg.host = "other";  // OK
cfg.port = 8080;     // ERROR: cannot assign to readonly property
```

`Readonly<T>` marks all fields readonly.

### Template Literal Types

Template literal types validate string patterns at build time:

```typescript
type ApiRoute = `/api/${string}`;
const good: ApiRoute = "/api/users";   // OK
const bad: ApiRoute = "/other";        // ERROR
```

### Literal Types and Annotation Semantics

`const` bindings preserve their literal type (`const x = 200` has type `200`). `let` bindings without explicit annotations are widened to the base type (`let x = 200` has type `number`), so `x = 404` compiles without error.

When a `const` binding has a base primitive annotation, the compiler validates assignability but keeps the narrower literal type:

```typescript
const port: number = 3000;  // type is 3000, validated against number
const bad: number = "oops"; // ERROR: string not assignable to number
```

For union annotations, the declared type is preserved to support exhaustiveness checking in `match` expressions.

### Generic Type Aliases

Generic type aliases (`type Result<T> = { ok: boolean; value: T }`) are instantiated when used in annotations. `Result<string>` resolves to `{ ok: boolean; value: string }` for structural checking. Up to 8 type parameters per alias.

---

## Compile-Time Evaluation

The `comptime()` function (`zigts/comptime.zig`) evaluates expressions at compile time and replaces them with literal values. It integrates with the type stripper as a pre-parse transformation.

### Usage

```typescript
const x = comptime(1 + 2 * 3);                 // -> const x = 7;
const upper = comptime("hello".toUpperCase()); // -> const upper = "HELLO";
const etag = comptime(hash("content-v1"));     // -> const etag = "a1b2c3d4";
const pi = comptime(Math.PI);                  // -> const pi = 3.141592653589793;
const cfg = comptime({ timeout: 30 });         // -> const cfg = ({timeout:30});
const region = comptime(Env.AWS_REGION);       // -> const region = "us-east-1";
```

### Supported Operations

**Literals**: number, string, boolean, `null`, `undefined`, `NaN`, `Infinity`

**Operators**: `+ - * / % **`, `| & ^ << >> >>>`, `== != === !== < <= > >=`, `&& || ??`, `? :`, `+ - ! ~` (unary)

**Arrays and objects**: `[1, 2, 3]`, `{ a: 1, b: "x" }` (comptime values only)

**Math constants**: `Math.PI`, `Math.E`, `Math.LN2`, `Math.LN10`, `Math.LOG2E`, `Math.LOG10E`, `Math.SQRT2`, `Math.SQRT1_2`

**Math functions**: `abs`, `floor`, `ceil`, `round`, `trunc`, `sqrt`, `cbrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `log`, `log2`, `log10`, `exp`, `pow`, `min`, `max`, `sign`, `clz32`, `imul`, `fround`, `hypot`

**String properties**: `length`

**String methods**: `toUpperCase()`, `toLowerCase()`, `trim()`, `trimStart()`, `trimEnd()`, `slice()`, `substring()`, `includes()`, `startsWith()`, `endsWith()`, `indexOf()`, `charAt()`, `split()`, `repeat()`, `replace()`, `replaceAll()`, `padStart()`, `padEnd()`

**Built-in functions**: `parseInt()`, `parseFloat()`, `JSON.parse()`, `hash()` (FNV-1a, returns 8-char hex)

**Environment variables**: `Env.VARNAME` (configured via `StripOptions.comptime_env`)

**Build metadata**: `__BUILD_TIME__`, `__GIT_COMMIT__`, `__VERSION__`

### Disallowed Operations

Variables, arbitrary function calls, `Date.now()`, `Math.random()`, `new`, `this`, `eval`, assignments, loops, closures.

### Error Types

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

### Performance Guards

- Max expression length: 8KB
- Max AST depth: 64

---

## Implementation Details

### Files

- `zigts/stripper.zig` - Type stripper with comptime integration
- `zigts/comptime.zig` - Compile-time expression evaluator (~2000 lines)
- `StripOptions` controls features: `tsx_mode`, `enable_comptime`, `comptime_env`

### Build-Time Integration

The stripper runs as a prepass for `.ts` and `.tsx` sources before zigts parsing. Runtime parser receives JS-only output. Enable comptime via `StripOptions.enable_comptime`.
