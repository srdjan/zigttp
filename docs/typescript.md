# TypeScript Support

zigttp includes native TypeScript and TSX support through two features: a type stripper that removes type annotations at load time, and a compile-time evaluator for the `comptime()` function.

---

## Type Stripper

The type stripper (`zts/stripper.zig`) removes TypeScript syntax before parsing, preserving line/column positions for error reporting by replacing stripped spans with spaces.

### Supported Subset

**Type declarations** (stripped entirely):
- `type` aliases (including ADT unions)
- `interface` declarations
- `export type ...` / `import type ...`

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

## Compile-Time Evaluation

The `comptime()` function (`zts/comptime.zig`) evaluates expressions at compile time and replaces them with literal values. It integrates with the type stripper as a pre-parse transformation.

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

- `zts/stripper.zig` - Type stripper with comptime integration
- `zts/comptime.zig` - Compile-time expression evaluator (~2000 lines)
- `StripOptions` controls features: `tsx_mode`, `enable_comptime`, `comptime_env`

### Build-Time Integration

The stripper runs as a prepass for `.ts` and `.tsx` sources before zts parsing. Runtime parser receives JS-only output. Enable comptime via `StripOptions.enable_comptime`.
