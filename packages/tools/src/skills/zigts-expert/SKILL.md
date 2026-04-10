---
name: zigts-expert
description: Write handler code in the zigts TypeScript subset for zigttp's serverless runtime. Covers the language spec, virtual modules, compile-time verification, sound mode type safety, and idiomatic FaaS patterns. Use for any .ts/.tsx/.js/.jsx files targeting zigttp.
---

# zigts-expert - Skill + Compiler-in-the-Loop

## Architecture

```
1. Read this SKILL.md (language expertise)
2. Write zigts code using the rules below
3. Run: zigts check --json handler.ts
4. Read structured JSON from stdout
5. Fix errors using suggestion fields
6. Repeat until success
```

No MCP server. No protocol. No config wiring. Just a binary and a skill.

## When to Use

Trigger when: writing `.ts` files for zigttp handlers, compiling with `zigts`, debugging zigts compiler errors, or asking about the zigts language subset.

## CLI Reference

```bash
# Verify handler and show proof card (human-readable)
zigts check handler.ts

# Verify with structured JSON output (use this in the loop)
zigts check --json handler.ts

# Compile handler to bytecode
zigts compile handler.ts output.zig

# List what's allowed and what's blocked
zigts features
zigts features --json

# List available virtual modules
zigts modules
zigts modules --json

# Show proof report (env vars, hosts, sandbox contract)
zigts check --json --contract handler.ts

# Contract comparison
zigts prove old-contract.json new-contract.json
```

**Always use `--json` when running from Claude Code.** Parse the result. Never guess at errors from unstructured stderr.

## Compiler-in-the-Loop Workflow

When writing or fixing zigts code, follow this exact loop:

```
1. Write the handler using the rules in this skill
2. Run: zigts check --json <file>
3. If success -> done, report the proof summary to the user
4. If errors -> read each diagnostic's `suggestion` field
5. Apply the suggestions (don't guess - the compiler knows the idiom)
6. Go to 2
```

### Reading Compiler Output

Success:
```json
{
  "success": true,
  "proof": {
    "env_vars": ["JWT_SECRET"],
    "outbound_hosts": ["api.stripe.com"],
    "virtual_modules": ["zigttp:auth", "zigttp:cache"],
    "properties": {
      "retry_safe": true,
      "idempotent": false,
      "injection_safe": true,
      "deterministic": false,
      "read_only": false,
      "state_isolated": true,
      "fault_covered": true
    }
  },
  "diagnostics": []
}
```

Error:
```json
{
  "success": false,
  "diagnostics": [
    {
      "code": "ZTS001",
      "severity": "error",
      "message": "'try/catch' is not supported",
      "file": "handler.ts",
      "line": 23,
      "column": 3,
      "suggestion": "use Result types for error handling"
    }
  ]
}
```

**The `suggestion` field is authoritative.** Follow it. Don't invent an alternative.

### Diagnostic Code Ranges

- ZTS0xx: Parser errors (unsupported features, syntax)
- ZTS1xx: Sound mode / BoolChecker (type safety in conditions, arithmetic)
- ZTS2xx: TypeChecker (type mismatches, argument errors)
- ZTS3xx: HandlerVerifier (missing returns, unchecked results, state isolation)

## Language Rules - What zigts Is

zigts is a restricted TypeScript subset compiled by a Zig-based compiler. It removes features to guarantee that every handler is a pure function with a statically provable sandbox.

### The Core Invariant

The IR tree IS the control flow graph. No cycles, no hidden exception paths, no non-local jumps. Every feature that would break this property is blocked.

### What's Allowed

| Category | Features |
|----------|----------|
| Declarations | `let`, `const`, `function`, arrow functions, destructuring (array/object/rest) |
| Control flow | `if`/`else`, `for...of` with `break`/`continue`, `return`, `assert` |
| Expressions | Template literals, ternary, spread, optional chaining (`?.`), nullish coalescing (`??`) |
| Operators | `+` `-` `*` `/` `%` `**`, `===` `!==` `<` `>` `<=` `>=`, `&&` `||` `!` |
| Assignment | `=` `+=` `-=` `*=` `/=` `%=` `**=` |
| Modules | `import { x } from "zigttp:mod"`, `import { x } from "./local"`, `export` |
| Special | `match` expression, pipe operator `|>`, `guard()` composition, `comptime()` |
| Types | Type aliases, `distinct type`, interfaces, annotations, `readonly` fields, type guards (`x is T`), template literal types |

### What's Blocked (and Why)

| Banned | Use Instead |
|--------|-------------|
| `switch`/`case` | `match` expression |
| `class` | Plain objects and functions |
| `while`, `do...while` | `for (const x of range(n))` or `for (const x of collection)` |
| C-style `for (;;)` | `for (const i of range(n))` |
| `for...in` | `for (const k of Object.keys(obj))` |
| `try`/`catch`/`finally` | Result types: check `.ok` before `.value` |
| `throw` | `return Response.json({ error: "..." }, { status: 500 })` |
| `var` | `let` or `const` |
| `null` | `undefined` (sole absent-value sentinel) |
| `==`, `!=` | `===`, `!==` (strict equality only) |
| `++`, `--` | `x = x + 1`, `x = x - 1` |
| Regular expressions | String methods: `includes`, `startsWith`, `endsWith`, `indexOf` |
| `new` (constructors) | Factory functions, object literals |
| `this` | Explicit parameter passing |
| `async`/`await`/`Promise` | `fetchSync()`, `parallel()`, `race()` |
| `delete` | `const { key, ...rest } = obj` |

### Error Handling

zigts has no try/catch. All errors flow through two patterns:

**Result types:** Functions like `jwtVerify`, `validateJson` return `{ ok: true, value: T } | { ok: false, error: string }`. The verifier enforces checking `.ok` before accessing `.value`.

```typescript
const result = jwtVerify(token, secret);
if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
const claims = result.value;
```

**Optional narrowing:** Functions like `env()`, `cacheGet()`, `parseBearer()` return `T | undefined`. Four recognized patterns:

| Pattern | Example |
|---------|---------|
| Truthiness guard | `if (val) { /* val is T */ }` |
| Early return | `if (!val) return ...; // val is T below` |
| Undefined check | `if (val !== undefined) { /* val is T */ }` |
| Nullish coalesce | `const v = val ?? "default"; // v is string` |

### Type Guards and Assert

Type guard functions declare narrowing with `x is T` return types. The `assert` statement applies the guard as permanent forward narrowing:

```typescript
function isString(x: unknown): x is string {
    return typeof x === "string";
}

// Branch narrowing
if (isString(val)) { val.toUpperCase(); }

// Forward narrowing (permanent from this point)
assert isString(val);
val.toUpperCase();

// With explicit error response
assert isString(name), Response.json({ error: "name required" }, { status: 400 });
```

Without an error expression, `assert` halts. With one, it returns that value.

### Discriminated Union Narrowing

Unions with a tag field narrow through `if` conditions:

```typescript
type Result = { kind: "ok", value: string } | { kind: "err", error: string };

if (r.kind === "err") {
    return Response.json({ error: r.error }, { status: 400 });
}
// r is narrowed to { kind: "ok", value: string }
```

### Distinct Types

`distinct type` creates nominal types. Values of different distinct types are incompatible even if they share the same base:

```typescript
distinct type UserId = string;
distinct type SessionId = string;

const uid: UserId = UserId("usr_123");
const sid: SessionId = SessionId("sess");
// uid and sid are not interchangeable
```

### Readonly and Template Literal Types

`readonly` fields reject assignment at compile time:

```typescript
type Config = { readonly port: number; host: string };
cfg.port = 8080;  // ERROR
```

Template literal types validate string patterns:

```typescript
type ApiRoute = `/api/${string}`;
const good: ApiRoute = "/api/users";   // OK
const bad: ApiRoute = "/other";        // ERROR
```

### Handler Structure

Every handler is a function named `handler` that receives a Request and returns a Response.

```typescript
function handler(req: Request): Response {
    return Response.json({ ok: true });
}
```

#### Request Properties

```typescript
req.method    // string: "GET", "POST", etc.
req.url       // string: full URL with query string
req.path      // string: URL path without query string
req.query     // object: parsed query parameters
req.body      // string | undefined: raw request body
req.headers   // object: header name -> value (lowercase keys)
req.params    // object: route parameters (after routerMatch)
```

#### Response Helpers

```typescript
Response.json(data, init?)       // application/json
Response.text(text, init?)       // text/plain
Response.html(html, init?)       // text/html
Response.redirect(url, status?)  // 302 by default
// init: { status: 201, statusText: "Created", headers: { "X-Custom": "val" } }
```

### Virtual Modules

All external capabilities come through `zigttp:*` virtual modules. Each `import` is a provable contract - the compiler records exactly which modules and bindings a handler uses.

Run `zigts modules` for the full list. Key modules:

```typescript
import { env } from "zigttp:env";
import { sha256, hmacSha256, base64Encode, base64Decode } from "zigttp:crypto";
import { routerMatch } from "zigttp:router";
import { parseBearer, jwtVerify, jwtSign } from "zigttp:auth";
import { schemaCompile, validateJson } from "zigttp:validate";
import { cacheGet, cacheSet, cacheIncr } from "zigttp:cache";
import { parallel, race } from "zigttp:io";
import { guard, pipe } from "zigttp:compose";
import { logInfo, logError } from "zigttp:log";
```

### Pattern Matching

The `match` expression matches values against object patterns. Every match must have a `default` arm.

```typescript
function handler(req: Request): Response {
    return match (req) {
        when { method: "GET", path: "/health" }:
            Response.json({ ok: true })
        when { method: "POST", path: "/echo" }:
            Response.json({ echo: req.body })
        default:
            Response.text("Not Found", { status: 404 })
    };
}
```

### Composition with pipe and guard

```typescript
import { guard } from "zigttp:compose";

const rateLimiter = (req: Request): Response | undefined => {
    const ip = req.headers["x-forwarded-for"] ?? "unknown";
    const count = cacheIncr("ratelimit", ip, 1, 60);
    if (count > 100) return Response.json({ error: "rate limited" }, { status: 429 });
};

const requireAuth = (req: Request): Response | undefined => {
    const token = parseBearer(req.headers["authorization"]);
    if (!token) return Response.json({ error: "unauthorized" }, { status: 401 });
    const result = jwtVerify(token, env("JWT_SECRET") ?? "secret");
    if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
};

const handler = guard(rateLimiter) |> guard(requireAuth) |> dashboard;
```

### Sound Mode Type System

The BoolChecker enforces type-directed safety rules at compile time:

- Boolean contexts: only boolean, number, string, optional types accepted (not objects or functions)
- Arithmetic: both operands must be numbers
- Addition: `number + number` or `string + string` only - mixed types are errors (use template literals)
- Optionals: must be narrowed before use in arithmetic or addition

### Built-in Globals

`Object.keys/values/entries`, array HOFs (`map`, `filter`, `reduce`, `find`, `some`, `every`, `forEach`), all `String` methods, `Math`, `JSON.parse/stringify`, `Date.now()`, `console.log`, `range(end)`, `fetchSync(url, init?)`, `parseInt`, `parseFloat`, `structuredClone`.

### Compile-Time Features

- `comptime(expr)`: evaluate at load time (literals, arithmetic, Math, string methods, hash)
- `-Dverify`: proves exhaustive returns, Result checking, optional narrowing, state isolation
- `-Dcontract`: extracts sandbox contract (env vars, hosts, modules, properties)

## Writing Good zigts Code

1. Start with the handler signature: `function handler(req: Request): Response`
2. Import only what you need - every import is a sandbox contract
3. Use `const` for everything - there is no mutation needed
4. Use Result types for fallibility - never throw
5. Use `pipe()` for sequencing - never nest callbacks
6. Use `match()` for routing - never chain if/else on method+path
7. Type your functions - annotations drive the type checker and proof system
8. One handler per file - the compiler's proof unit is a single file

## When the Compiler Knows Best

If `zigts check --json` returns an error with a suggestion, **use the suggestion**. The compiler understands the restricted grammar better than any language model. The skill gives you the patterns; the compiler enforces them.

## Reference Files

- `references/virtual-modules.md` - Full API documentation for all `zigttp:*` modules
- `references/testing-replay.md` - JSONL test format, deterministic replay, build-time verification
- `references/jsx-patterns.md` - JSX/TSX component patterns and SSR rendering
