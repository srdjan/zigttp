---
name: zigts-expert
description: "Write handler code in the zigts TypeScript subset for zigttp's serverless runtime. Covers the language spec, virtual modules, compile-time verification, sound mode type safety, and idiomatic FaaS patterns. Use for any .ts/.tsx/.js/.jsx files targeting zigttp."
---

# zigts-expert - Skill + Compiler-in-the-Loop

## When to Use

Trigger when: writing `.ts` files for zigttp handlers, compiling with `zigts`, debugging zigts compiler errors, or asking about the zigts language subset. Uses a compiler-in-the-loop workflow: write code, run `zigts check --json`, fix from diagnostics, repeat.

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

## Language Rules

### What's Allowed

`let`/`const`/`function`/arrows, destructuring, `if`/`else`, `for...of`, `return`, `assert`, template literals, ternary, spread, `?.`, `??`, standard operators (`===`/`!==`, arithmetic, logical), compound assignments, `import`/`export`, `match` expression, pipe `|>`, `guard()`, `comptime()`. Type system: aliases, `distinct type`, interfaces, annotations, `readonly`, type guards (`x is T`), template literal types.

### What's Blocked (and Why)

| Banned | Use Instead |
|--------|-------------|
| `switch`/`case` | `match` expression |
| `class`, `new`, `this` | Plain objects, factory functions, explicit params |
| `while`, `do...while`, C-style `for`, `for...in` | `for (const x of range(n))` or `for (const x of collection)` / `Object.keys(obj)` |
| `try`/`catch`/`finally`, `throw` | Result types (check `.ok` before `.value`) or error responses |
| `var` | `let` or `const` |
| `null` | `undefined` (sole absent-value sentinel) |
| `==`, `!=`, `++`, `--` | `===`/`!==` (strict only), `x = x + 1` |
| Regular expressions | String methods: `includes`, `startsWith`, `endsWith`, `indexOf` |
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

### Built-in Globals

`Object.keys/values/entries`, array HOFs (`map`/`filter`/`reduce`/`find`/`some`/`every`/`forEach`), `String` methods, `Math`, `JSON.parse/stringify`, `Date.now()`, `console.log`, `range(end)`, `fetchSync(url, init?)`, `parseInt`/`parseFloat`, `structuredClone`.

### Compile-Time Features

`comptime(expr)` evaluates at load time. `-Dverify` proves exhaustive returns, Result checking, optional narrowing, state isolation. `-Dcontract` extracts sandbox contract (env vars, hosts, modules, properties).

## Reference Files

- `references/virtual-modules.md` - Full API documentation for all `zigttp:*` modules
- `references/testing-replay.md` - JSONL test format, deterministic replay, build-time verification
- `references/jsx-patterns.md` - JSX/TSX component patterns and SSR rendering
