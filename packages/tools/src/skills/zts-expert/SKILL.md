---
name: zts-expert
description: Write handler code in the zts TypeScript subset for zttp's serverless runtime. Covers the language spec, virtual modules, compile-time verification, sound mode type safety, and idiomatic FaaS patterns. Use for any .ts/.tsx/.js/.jsx files targeting zttp.
---

# zts-expert - Skill + Compiler-in-the-Loop

## Architecture

```
1. Read this SKILL.md (language expertise)
2. Write zts code using the rules below
3. Run: zts check --json handler.ts
4. Read structured JSON from stdout
5. Fix errors using suggestion fields
6. Repeat until success
```

No MCP server. No protocol. No config wiring. Just a binary and a skill.

## When to Use

Trigger when: writing `.ts` files for zttp handlers, compiling with `zts`, debugging zts compiler errors, or asking about the zts language subset.

## CLI Reference

```bash
# Verify handler and show proof card (human-readable)
zts check handler.ts

# Verify with structured JSON output (use this in the loop)
zts check --json handler.ts

# Compile handler to bytecode
zts compile handler.ts output.zig

# List what's allowed and what's blocked
zts features
zts features --json

# List available virtual modules
zts modules
zts modules --json

# Show proof report (env vars, hosts, sandbox contract)
zts check handler.ts --json

# Emit contract.json in the current directory
zts check handler.ts --contract

# Preview compiler-authored local canonical refactors
zts canonicalize handler.ts --json --simulate

# Contract comparison
zts prove old-contract.json new-contract.json
```

**Always use `--json` when running from an interactive expert client.** Parse the result. Never guess at errors from unstructured stderr.

## REPL (`zttp expert`)

The interactive agent. Natural language is sent to the model by default. Slash commands run locally without a model call.

### Slash Commands

| Command | Description |
|---|---|
| `/meta` | Compiler version, policy hash |
| `/features` | Allowed/blocked language features |
| `/modules` | Virtual module exports |
| `/rule <code>` | Explain a diagnostic code (e.g. ZTS303) |
| `/search <keyword>` | Search all rules by keyword |
| `/verify <path...>` | Check files for violations |
| `/check <path>` | Full handler analysis |
| `/build <step>` | Run a zig build step |
| `/test [step]` | Run tests |
| `/resume` | Resume the latest session for this cwd |
| `/new` | Start a fresh session |
| `help` or `:h` | Show command reference |
| `quit`, `exit`, or `:q` | Exit the REPL |

Explicit `zts` subcommands (`zts meta`, `zts features`, etc.) also dispatch locally.

### Launch Flags

```bash
# Approval
zttp expert --yes           # auto-approve all verified edits
zttp expert --no-edit       # auto-reject all verified edits

# Session
zttp expert --session-id <id>           # named session (resume or create)
zttp expert --resume                    # resume newest session for this cwd
zttp expert --no-session                # disable session persistence
zttp expert --no-persist-tool-output    # omit tool output from session log

# Non-interactive
zttp expert --print "add a GET /health route"     # one-shot, plain text output
zttp expert --print "..." --mode json             # one-shot, NDJSON events to stdout
```

### `--mode json` Event Format

With `--print <prompt> --mode json`, the agent emits one NDJSON line per event then exits. This is the same shape as `events.jsonl` session files, so one parser handles both.

```json
{"v":1,"k":"user_text","d":"add a GET /health route"}
{"v":1,"k":"tool_use","d":{"id":"tu_01","name":"zts_check","args_json":"{\"path\":\"handler.ts\"}"}}
{"v":1,"k":"tool_result","d":{"tool_use_id":"tu_01","tool_name":"zts_check","ok":true,"body":"..."}}
{"v":1,"k":"model_text","d":"Added the route. Proof: retry_safe=true, idempotent=true."}
{"v":1,"k":"end"}
```

Event kinds: `user_text`, `model_text`, `tool_use`, `tool_result`, `proof_card`, `diagnostic_box`, `end`.
Field `v` is the schema version (currently `1`). The `end` event always appears last.

## Compiler-in-the-Loop Workflow

When writing or fixing zts code, follow this exact loop:

```
1. Write the handler using the rules in this skill
2. Run: zts check --json <file>
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
    "virtual_modules": ["zttp:auth", "zttp:cache"],
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

To record I/O for deterministic replay and test generation, see `references/testing-replay.md` (`--trace`, `--replay`, JSONL test format).

## One-Way ZigTS

The one-way profile is the default strict profile for code that should remain
easy for the compiler and agent to refactor mechanically. ZTS608+ diagnostics
are errors, not advisory style warnings.

Rules:

- Prefer named `function` declarations for reused helpers. Keep arrow
  functions for callbacks and one-off local values.
- Prefer `export function name(...)` over exported function-valued `const`
  declarations unless the export is intentionally a first-class value.
- Public helpers that reach capabilities should declare an explicit
  `Effects<...>` return capsule.
- Public helpers used under declared specs should declare an explicit
  `Proof<...>` return capsule.

Canonical diagnostics start at ZTS608. Existing strict diagnostics keep their
current meanings: ZTS602 for dynamic capability keys, ZTS604 for avoidable
`let`, and ZTS606/ZTS607 for proof/effects failures.

Use `zts canonicalize <file> --json` to preview compiler-authored local
rewrite intents for canonical diagnostics and mechanically safe strict-profile
repairs such as `let` to `const`. Add `--simulate` to have the compiler apply
the previews in memory and run `edit-simulate`; source files are never written.
Apply the replacement only through `edit-simulate` / compiler veto.

### Diagnostic Code Ranges

- ZTS0xx: Parser errors (unsupported features, syntax)
- ZTS1xx: Sound mode / BoolChecker (type safety in conditions, arithmetic)
- ZTS2xx: TypeChecker (type mismatches, argument errors)
- ZTS3xx: HandlerVerifier (missing returns, unchecked results, state isolation)
- ZTS6xx: Strict and one-way ZigTS diagnostics

## Language Rules - What zts Is

zts is a restricted TypeScript subset compiled by a Zig-based compiler. It removes features to guarantee that every handler is a pure function with a statically provable sandbox.

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
| Modules | `import { x } from "zttp:mod"`, `import { x } from "./local"`, `export` |
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
| `delete` | New object literal with only the keys you keep |

### Error Handling

zts has no try/catch. All errors flow through two patterns:

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

All external capabilities come through `zttp:*` virtual modules. Each `import` is a provable contract - the compiler records exactly which modules and bindings a handler uses.

Run `zts modules` for the full list. Key modules:

```typescript
import { env } from "zttp:env";
import { sha256, hmacSha256, base64Encode, base64Decode } from "zttp:crypto";
import { routerMatch } from "zttp:router";
import { parseBearer, jwtVerify, jwtSign } from "zttp:auth";
import { schemaCompile, validateJson } from "zttp:validate";
import { cacheGet, cacheSet, cacheIncr } from "zttp:cache";
import { parallel, race } from "zttp:io";
import { guard, pipe } from "zttp:compose";
import { logInfo, logError } from "zttp:log";
import { serviceCall } from "zttp:service";
import { send, close, getWebSockets, setAutoResponse, serializeAttachment, deserializeAttachment } from "zttp:websocket";
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
import { guard } from "zttp:compose";

function rateLimiter(req: Request): Response | undefined {
    const ip = req.headers["x-forwarded-for"] ?? "unknown";
    const count = cacheIncr("ratelimit", ip, 1, 60);
    if (count > 100) return Response.json({ error: "rate limited" }, { status: 429 });
}

function requireAuth(req: Request): Response | undefined {
    const token = parseBearer(req.headers["authorization"]);
    if (!token) return Response.json({ error: "unauthorized" }, { status: 401 });
    const secret = env("JWT_SECRET");
    if (secret === undefined) return Response.json({ error: "server misconfigured" }, { status: 500 });
    const result = jwtVerify(token, secret);
    if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
}

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

### Workflow Authoring

Durable workflow code is a disciplined composition of existing modules, not a
new DSL. For the copyable first shape, import `run` from `zttp:durable` and
`call` from `zttp:workflow`, derive the run key from
`req.headers.get("idempotency-key")`, and put the top-level `workflow.call`
inside `run()`. Use `step()` for replayable JSON-snapshot work only. Because
workflow modules are write-effect modules, declare a narrow `Spec<...>` with
only the properties `zts check --json` proves.

Never put `workflow.call`, `saga`, `fanout`, or `follow` inside a durable
`step()` callback; ZTS509 rejects that shape. For `saga([...])`, every non-last
static step needs a `compensate` function or ZTS510 rejects it. `fanout()` is
ordered durable batch grouping, not true concurrency.

## Writing Good zts Code

1. Start with the handler signature: `function handler(req: Request): Response`
2. Import only what you need - every import is a sandbox contract
3. Use `const` for everything - there is no mutation needed
4. Use Result types for fallibility - never throw
5. Use `pipe()` for sequencing - never nest callbacks
6. Use `match()` for routing - never chain if/else on method+path
7. Type your functions - annotations drive the type checker and proof system
8. One handler per file - the compiler's proof unit is a single file

## When the Compiler Knows Best

If `zts check --json` returns an error with a suggestion, **use the suggestion**. The compiler understands the restricted grammar better than any language model. The skill gives you the patterns; the compiler enforces them.

## JSX/SSR

Use `.tsx` files for server-side HTML rendering. There is no client-side hydration. `renderToString` is the only rendering entry point.

```tsx
type Todo = { id: number; text: string; done: boolean };

function TodoItem(props: { todo: Todo }): JSX.Element {
    const cls = props.todo.done ? "done" : "pending";
    return <li class={cls}>{props.todo.text}</li>;
}

function TodoList(props: { todos: Todo[] }): JSX.Element {
    return (
        <ul>
            {props.todos.map((t) => <TodoItem todo={t} />)}
        </ul>
    );
}

function handler(req: Request): Response {
    const todos: Todo[] = [
        { id: 1, text: "ship it", done: false },
    ];
    const html = renderToString(<TodoList todos={todos} />);
    return Response.html(html);
}
```

See `references/jsx-patterns.md` for the full component API, attribute rules, and fragment syntax.

## Reference Files

- `references/virtual-modules.md` - Full API documentation for all `zttp:*` modules
- `references/testing-replay.md` - JSONL test format, deterministic replay, build-time verification
- `references/jsx-patterns.md` - JSX/TSX component patterns and SSR rendering
