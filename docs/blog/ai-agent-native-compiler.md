---
title: "The First AI-Agent-Native Compiler"
date: 2026-04-23
tags: [zigts, zigttp, ai-agents, compiler-design, developer-tools]
excerpt: AI agents are prolific and unreliable in equal measure. The fix isn't better models — it's a language with fewer wrong choices and a compiler that proves the rest. zigts is that compiler.
---

AI coding agents are prolific and unreliable in equal measure.

Not because the models are bad. Because the languages are wide. TypeScript gives an agent 54 ways to express a loop, define a function, or handle an error — and exactly one of those ways is consistent with the rest of the codebase. The agent doesn't know which one. It guesses. Sometimes it guesses well.

The conventional answer is guardrails: linters, formatters, code review, test suites. All of these assume the code already exists and try to catch problems after the fact. That's the wrong layer.

The right layer is the language itself.

## 57 Features Removed, Zero Capabilities Lost

zigts is a TypeScript compiler that ships with 57 features removed at parse time — each with a one-line error message pointing to the one way that remains. Not deprecated. Not warned. Removed. The parser rejects them before a single byte of IR is emitted.

Here is a partial list of what's gone and what replaced it:

| Removed | One Way That Remains |
|---------|---------------------|
| `while`, `do-while`, C-style `for`, `for-in` | `for (const x of collection)` |
| `try`, `catch`, `throw` | Result types: `{ ok, value }` or `{ ok, error }` |
| `class`, `new`, `this`, `super` | Plain objects and functions |
| `null` | `undefined` as sole absent-value sentinel |
| `==`, `!=` | `===`, `!==` |
| `var` | `let`, `const` |
| `++`, `--` | `x = x + 1` |
| `export default`, `import *`, re-exports | Named imports and exports only |
| Function expressions | Arrow functions or function declarations |
| `switch` / `case` / `break` | `match` expression with exhaustiveness checking |

Every removal is a decision an AI agent no longer has to make. Every remaining construct is the *only* way to express that idea. The grammar has already decided. An agent generating zigts code can't produce a style inconsistency — there's only one style.

## The Handler Contract

A zigts handler is a pure function: `Request` in, `Response` out. No middleware chains. No class hierarchies. No lifecycle hooks. One function, one file, one contract.

The only way to access the outside world is through virtual modules:

```typescript
import { jwtVerify } from "zigttp:auth";
import { validateJson } from "zigttp:validate";
import { cacheGet, cacheSet } from "zigttp:cache";
import { env } from "zigttp:env";
```

These aren't npm packages. They're compiler-known capabilities. Each one has a typed signature, a declared effect class (read, write, or none), and data provenance labels that the compiler tracks through every operation. There is no `node_modules`. There is no supply chain. There are seventeen virtual modules, and they are the entire surface area between a handler and the world.

This is isomorphic to how AI agents reason about tools. An agent with access to a `search` tool and a `calculator` tool decides which tool to use and what arguments to pass. A zigts handler with access to `zigttp:cache` and `zigttp:auth` is the same structure. The virtual modules are tool declarations. The compiler enforces correct usage. The contract is the capability boundary.

## Six Things the Compiler Proves

When you compile with `zig build -Dverify`, the compiler doesn't just check types. It walks every branch of the handler and proves six properties:

**1. Response completeness.** Every code path returns a Response. The diagnostic is exact:

```
not all code paths return a Response
```

**2. Result safety.** Every Result from `jwtVerify`, `validateJson`, `cacheGet` is checked before its value is accessed:

```
result.value accessed without checking result.ok first
Fix: check result.ok before accessing result.value:
     if (result.ok) { ... result.value ... }
```

The compiler knows which virtual module produced the Result and which function returned it.

**3. Optional safety.** Every optional value from a virtual module is narrowed before use:

```
optional value used without checking for undefined
property access on optional value without checking for undefined
```

**4. State isolation.** No handler mutates module-scope state. Mutable module-scope variables break cross-request isolation in a pooled runtime:

```
handler mutates module-scope variable (breaks cross-request isolation)
Fix: use const instead of let for module-scope declarations, or move state to zigttp:cache
```

**5. Secret and credential leakage.** The compiler labels every value with its data provenance — a 7-bit set tracking `secret`, `credential`, `user_input`, `config`, `internal`, `external`, `validated`. Values derived from `env("JWT_SECRET")` carry the `secret` label. Values from `jwtVerify` carry `credential`. If any `secret`-labeled value reaches a Response body, the compiler emits:

```
SECRET LEAKAGE  handler.ts:31
    secret-derived value reaches response body
    Fix: remove secret data from response
```

**6. Fault coverage.** The compiler enumerates every failure path (what happens when `cacheGet` returns undefined? when `jwtVerify` returns `{ ok: false }`?) and verifies the handler returns an appropriate error Response for each. Uncovered faults produce counterexample test cases:

```
FAULT COVERAGE GAP
    handler returns 2xx when auth fails
    Fix: return 401 on failure
    Counterexample: "path 1 !result.ok" (see handler.violations.jsonl)
```

These aren't lint warnings. They're proof failures. The compiler walked the code, found a path that violates a property, and generated a concrete input that demonstrates the bug. The counterexample can be replayed.

For an AI agent, this changes the development loop from:

```
generate → run tests → read failure → guess fix → repeat
```

to:

```
generate → compile → read proof failure → targeted fix → done
```

The compiler tells the agent exactly what's wrong, exactly where, and exactly how to fix it. Not "test on line 47 failed." A named property, a source location, a fix suggestion, and a counterexample.

## The Contract Artifact

When you compile with `zig build -Dcontract`, the compiler extracts a machine-readable contract:

```json
{
  "modules": ["zigttp:auth", "zigttp:cache", "zigttp:validate"],
  "env": { "literal": ["JWT_SECRET", "DATABASE_URL"], "dynamic": false },
  "routes": [{ "method": "GET", "path": "/api/users/:id" }],
  "egress": { "hosts": ["api.stripe.com"], "dynamic": false },
  "properties": {
    "stateless": true,
    "no_secret_leakage": true,
    "no_credential_leakage": true,
    "has_egress": true
  }
}
```

This is not documentation. It's a proof artifact. The `stateless` field means the compiler walked every statement and proved the handler never mutates module-scope state. The `no_secret_leakage` field means the compiler tracked every `secret`-labeled value and proved none reach a response body. The contract is what the compiler proved, emitted as JSON.

Deploy pipelines can consume it directly. A CI gate can reject handlers with `"no_secret_leakage": false`. A platform can sandbox handlers to the egress hosts declared in their contract. An orchestrator can diff contracts across versions and detect breaking changes.

For AI agents generating handlers, the contract is a checkable specification. The agent can read the contract, compare it against requirements, and verify its own output without running a single test.

## Exhaustive Branching

zigts replaces `switch`/`case`/`break` with a `match` expression. The compiler proves exhaustiveness: every variant of a discriminated union must be handled.

```typescript
const response = match (result) {
  { kind: "ok" } => Response.json(result.value),
  { kind: "err" } => Response.json({ error: result.error }, { status: 400 }),
};
```

Add a new variant to the union and forget to handle it? Compile error. Not a runtime crash two weeks later when a user hits the unhandled case.

For agents, exhaustiveness checking eliminates an entire class of omission errors. The agent generates the match, the compiler reports the missing arms, the agent fills them in. The compiler is a collaborator that never misses a case.

## Why Constraints Are the Feature

The conventional wisdom is that a more expressive language is a more powerful language. zigts inverts this. Every removed feature is a removed decision point. Every removed decision point is a removed source of inconsistency. Every remaining construct carries semantic weight — there's no stylistic noise.

The principle is Zig's: **one obvious way to do things**. Applied to a TypeScript subset, it produces something unexpected — a language that's better for AI agents *and* better for humans, for the same reason. The decisions that were eliminated were never meaningful. They were tax.

A human developer choosing between `while` and `for...of` is making a style decision. An AI agent making that choice is flipping a coin. Neither decision carries information.

A human developer choosing between `match` and an `if` chain is making a *semantic* decision: do I need exhaustiveness, or do I need early return? That choice carries information. It tells the next reader — human or AI — something about the intent.

zigts keeps every choice that carries information. It removes every choice that doesn't.

## Try It

```bash
zig build -Dhandler=handler.ts -Dverify    # Prove properties
zig build -Dhandler=handler.ts -Dcontract  # Extract contract
zig build run -- handler.ts -p 3000        # Run it
```

The compiler is the guardrail. The contract is the documentation. The proofs replace the tests you only wrote to defend against the language.

And for the AI agent writing your next handler, one obvious way to do everything is the only way that works.
