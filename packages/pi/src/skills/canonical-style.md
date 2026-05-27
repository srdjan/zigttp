---
name: canonical-style
description: Write handler code in the one-way ZigTS profile. One canonical spelling per operation; the compiler rejects the rest.
---
ZigTS already cuts most of TypeScript. The one-way profile cuts further: for every operation there is exactly one canonical spelling, and the compiler rejects the alternates as ZTS6xx errors. Before writing or rewriting a handler, consult `zigts_expert_describe_rule` and `zigts_expert_features` for the live rule set rather than answering from memory.

## Canonical forms

| Task | Canonical form |
|---|---|
| Handler / helper declaration | Named `function name(...): Return { ... }` with explicit parameter and return types |
| Local callback | Arrow function only when passed directly as a value |
| Binding | `const` by default; `let` only when reassigned |
| Absent value | `undefined` only |
| Branching | `if`/`else` for guards, `match` for closed alternatives - no ternary |
| Iteration | `for (const item of items)` over a finite collection |
| Errors | `Result<T>` values plus explicit `.ok` checks |
| External effects | `Effects<T, "...">` on public helpers that touch capabilities |
| Proof obligations | `Spec<...>` on handlers; `Proof<T, "...">` on helpers that participate in declared proofs |
| Module imports | Named imports from a literal `zigttp:*` or registered `zigttp-ext:*` specifier |
| Capability keys | String literals or compiler-visible `const` literal aliases |
| Arithmetic update | `x = x + 1`; never `x += 1` or `x++` |
| Function call args | Positional. No `f(...args)` spread |
| Object spread | Leading position only: `{...base, x: 1}` - never `{x: 1, ...base}` |
| Destructuring | One level deep; no rename. `const {a} = obj; const b = a;` not `const {a: b} = obj` |
| Default parameter | Explicit `undefined` check in the body, not `(a: T = default)` |
| Optional parameter | `(a: T | undefined)`, not `(a?: T)` |
| Template interpolation | `${identifier}` or `${obj.literalField}` only; hoist anything else to a `const` |
| Fallback | `??` for nullish defaults. Never `||` unless both operands are boolean |

Most of these rows have a corresponding `ZTS6xx` diagnostic and the compiler will reject violations. Three rows are advisory style only and not yet mechanically enforced: destructure rename (`{a: b}`), optional-parameter syntax (`(a?: T)`), and truthy `||` fallback. Write canonical code for them anyway - those checks will land in a later slice.

## Before / after pairs

### Avoidable `let` (ZTS604)
```ts
// before
let region = env("REGION") ?? "iad";
return Response.text(region);

// after
const region = env("REGION") ?? "iad";
return Response.text(region);
```

### Arrow helper that is reused (ZTS608)
```ts
// before
const parseUser = (input: Input): User => makeUser(input);

// after
function parseUser(input: Input): User {
  return makeUser(input);
}
```

### Exported function-valued `const` (ZTS609)
```ts
// before
export const handler = (req: Request): Response => Response.json({ok: true});

// after
export function handler(req: Request): Response {
  return Response.json({ok: true});
}
```

### Ternary expression (ZTS612)
```ts
// before
const status = ok ? 200 : 500;

// after - lift the choice into a named helper
function pickStatus(ok: boolean): number {
  if (ok) { return 200; }
  return 500;
}
const status = pickStatus(ok);
// or, when the result is returned directly:
if (ok) { return Response.json({}, {status: 200}); }
return Response.json({}, {status: 500});
```

### Compound assignment (ZTS613)
```ts
// before
count += 1;

// after
count = count + 1;
```

### Non-leading object spread (ZTS614)
```ts
// before
const next = {status: "ok", ...base};

// after
const next = {...base, status: "ok"};
```

### Complex template interpolation (ZTS615)
```ts
// before
return Response.text(`user ${getUser().name} at ${Date.now()}`);

// after
const user = getUser();
const now = Date.now();
return Response.text(`user ${user.name} at ${now}`);
```

### Spread in function call (ZTS616)
```ts
// before
return send(...args);

// after
return send(args[0], args[1], args[2]);
// or widen the signature: function send(args: SendArgs): Response
```

### Default parameter value (ZTS617)
```ts
// before
function greet(name: string = "world"): string {
  return `hello ${name}`;
}

// after - resolve the default in the body without using a ternary
function greet(name: string | undefined): string {
  let resolved: string;
  if (name === undefined) {
    resolved = "world";
  } else {
    resolved = name;
  }
  return `hello ${resolved}`;
}
```

### Nested destructuring (ZTS618)
```ts
// before
const {user: {name}} = payload;

// after
const {user} = payload;
const {name} = user;
```

## Working rules for the agent

1. Always consult live tools for rules and modules. Never answer language or module questions from memory: call `zigts_expert_describe_rule`, `zigts_expert_features`, `zigts_expert_modules`.
2. Run `zigts_expert_canonicalize` against any file you edit before claiming the edit is done. The veto path rejects anything that introduces new violations.
3. For optimizations, call `zigts_expert_effects` and `zigts_expert_ratchet` first, then include the proof-card delta in the reply. The loop emits a system note if you skip this step.
4. When the user asks about syntax that is rejected, cite the ZTS code and the canonical form from this skill.
