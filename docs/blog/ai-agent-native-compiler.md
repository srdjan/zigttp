---
title: "The First AI-Agent-Native Compiler"
date: 2026-04-11
tags: [zigts, zigttp, ai-agents, compiler-design, developer-tools]
excerpt: Every language designed before 2024 optimized for human developers writing code. zigts is the first compiler designed for a world where AI agents write most of it — and the constraints that make it agent-native are the same ones that make it better for humans.
---

Every programming language you use was designed for human beings sitting at keyboards.

The syntax serves human readability. The feature set serves human expressiveness. The error messages assume a person is reading them. The type systems balance strictness against the annoyance of a developer who just wants to ship.

None of that works when an AI agent is writing your code.

## The Agent Problem

AI coding agents — Claude, Cursor, Copilot, Devin, the next hundred — are stunningly productive. They are also stunningly unreliable. Not because the models are bad, but because the languages they target are bad *for them*.

Consider what a modern TypeScript codebase offers an AI agent:

- **34 ways to define a function** (declaration, expression, arrow, method, getter, setter, constructor, async variants of each, generator variants, class static methods...)
- **5 looping constructs** (`for`, `for...in`, `for...of`, `while`, `do...while`) plus recursion, plus `.forEach`, plus `.map`
- **2 equality operators** that produce different results on the same inputs
- **3 absent-value sentinels** (`null`, `undefined`, `void 0`)
- **Hidden control flow** via `try`/`catch` that makes any function call a potential branch
- **Implicit `this` binding** that changes behavior based on how a function is called, not how it's defined

Each choice point is a place where an agent can make the wrong decision. Not a logic error — a *style* error. The code works but it's inconsistent. Or it works in isolation but breaks when composed with human-written code that made different choices. Or it works now but chose a pattern that makes the next change harder.

Humans navigate this with taste, experience, and code review. Agents navigate it with probability distributions over training data. One of these approaches scales. Neither is reliable.

## What If the Language Had No Wrong Choices?

The Zig programming language has a design principle: **there should be one obvious way to do things**. No operator overloading (so `+` always means addition). No hidden allocators (so you see every allocation). No hidden control flow (so you can trace every branch).

zigts takes this principle and applies it to a JavaScript subset purpose-built for serverless handlers. The question isn't "what features should we add?" but "what features should we *remove* until there's exactly one way to express each idea?"

Here's what's left:

**One way to loop.** `for...of` over a finite collection. No `while` (can't express infinite loops). No `for-in` (confusing prototype chain walk). No C-style `for` (three-expression ceremony for a simple iteration). If you want to iterate 10 times: `for (const i of range(10))`. One pattern. Agents can't choose wrong.

**One way to define a function.** Well, two: declarations (hoisted) and arrows (not hoisted). But no function expressions, no methods-on-classes, no generators, no `this`-dependent behavior. A function is its parameters and its body. Period.

**One way to handle errors.** Result types. `{ ok: true, value: T }` or `{ ok: false, error: E }`. No `throw`. No `try/catch`. No hidden control flow. Every error path is visible in the return type, checkable by the compiler, provable at build time.

**One way to be absent.** `undefined`. No `null`. No `void 0`. One sentinel, one check.

**One equality operator.** `===`. Strict equality only. `==` is a parse error with a suggestion.

**One way to export.** Named exports. No `export default`. No re-exports. No barrel files.

The effect on AI agents isn't subtle. When an agent generates a handler in standard TypeScript, it makes dozens of style decisions per function — any of which might diverge from the codebase's conventions. When it generates a zigts handler, those decisions don't exist. The grammar has already made them.

## Constraints Are Capabilities

Removing features sounds like removing capability. It's the opposite. Each removed feature forces a pattern that the compiler can reason about:

**No `try`/`catch` means every error is a value.** The compiler tracks Result types across your handler, proves you checked `.ok` before accessing `.value`, and flags unchecked error paths at build time. An AI agent can't generate code that silently swallows errors — the compiler catches it before deployment.

**No `while` means every loop terminates.** `for...of` over a finite collection is provably terminating. The compiler doesn't need to solve the halting problem. It already knows.

**No `this` means no implicit context.** Functions are pure transforms from input to output. The compiler can isolate them, sandbox them, run them in parallel. An AI agent can't accidentally generate code where `this` refers to the wrong object — there is no `this`.

**No `class` means no inheritance hierarchy.** Data is plain objects. Behavior is plain functions. The compiler can see the full shape of every value at every point in the program. An AI agent can't generate a class hierarchy that becomes a refactoring bottleneck — there are no classes.

## The Compiler as Proof Engine

Here's where zigts diverges from "a linter with opinions" into something fundamentally different.

When you run `zig build -Dverify`, the compiler doesn't just check types. It *proves properties* about your handler:

**Response completeness.** Every code path returns a Response. Not "the types say it returns a Response" — the compiler walks every branch and proves it.

**Result checking.** Every Result from `jwtVerify`, `validateJson`, `cacheGet` is checked before its value is accessed. Unchecked Results are compile errors.

**State isolation.** No request can leak mutable state to another request. The compiler proves that handler functions are pure transforms of their input.

**Data flow safety.** Values labeled `secret` (env vars like `JWT_SECRET`) never reach a Response body. Values labeled `user_input` pass through validation before reaching a database query. The compiler tracks data provenance through every operation and every function call.

When you run `zig build -Dcontract`, the compiler extracts a machine-readable contract:

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

This contract isn't documentation. It's a *proof artifact*. The compiler extracted it by analyzing the code, and it guarantees the properties it claims.

For AI agents, this changes the development loop from:

```
generate code → run tests → read failures → guess fix → repeat
```

to:

```
generate code → compile → read proof failures → targeted fix → done
```

The compiler tells the agent exactly what's wrong — not "test on line 47 failed" but `result.value accessed without checking result.ok first` with the fix printed inline: `if (result.ok) { ... result.value ... }`. Or a `secret-leakage` diagnostic pointing at the exact value labeled `secret` that reached a Response body. The diagnostics are not generic type errors; they reference the specific property the compiler was trying to prove and the specific value that violated it.

## Virtual Modules Are Agent Tool Declarations

In standard serverless runtimes, accessing external resources means importing from npm, configuring clients, handling connection lifecycle, and hoping the AI agent generates the right incantation for each SDK.

In zigttp, the only way to touch the outside world is through virtual modules:

```typescript
import { cacheGet, cacheSet } from "zigttp:cache";
import { jwtVerify } from "zigttp:auth";
import { validateJson } from "zigttp:validate";
import { env } from "zigttp:env";
```

These aren't npm packages. They're compiler-known capabilities with typed signatures, declared effects, and proven contracts. The compiler knows that `cacheGet` is a read operation. It knows that `jwtVerify` returns a Result that must be checked. It knows that `env("JWT_SECRET")` produces a value labeled `secret`.

This is isomorphic to how AI agents think about tools. An agent with access to a `search` tool and a `calculator` tool reasons about which tool to use and what arguments to pass. A zigts handler with access to `zigttp:cache` and `zigttp:auth` is the same pattern — the virtual modules are the tool declarations, and the compiler enforces correct usage.

No npm. No `node_modules`. No supply chain. No configuration. Import the capability, use it correctly, or the compiler stops you.

## The `match` Expression: Exhaustiveness as Agent Guardrail

Most languages let you write a conditional chain and forget a case. zigts has one branching expression for value-dependent dispatch: `match`.

```typescript
const response = match (result) {
  { kind: "ok" } => Response.json(result.value),
  { kind: "err" } => Response.json({ error: result.error }, { status: 400 }),
};
```

This isn't syntactic sugar. The compiler proves exhaustiveness: every possible variant of the union must be handled. Add a new variant to your Result type and forget to handle it? Compile error. Not a runtime crash two weeks later when a user hits the unhandled case.

For AI agents generating handler code, exhaustiveness checking is transformative. The agent doesn't need to remember all the cases. It generates the match expression, the compiler tells it which arms are missing, and the agent fills them in. The compiler is a pair programmer that never misses a case.

## One Way to Do Things Is the AI-Native Way

The principle behind every design decision in zigts is the same one that makes it AI-agent-native: **reduce the decision space until the remaining choices are all meaningful**.

A human developer choosing between `while` and `for...of` is making a style decision. An AI agent making that choice is flipping a coin. Neither decision carries meaningful information — it's pure noise.

A human developer choosing between `match` and an `if` chain is making a *semantic* decision: "do I need exhaustiveness, or do I need early return?" That choice carries information. It tells the next reader (human or AI) something about the intent.

The zigts philosophy: eliminate every choice that doesn't carry information. What remains is a language where every syntactic decision is a semantic decision, every constraint is a capability, and every compilation is a proof.

The future isn't AI agents writing TypeScript and hoping for the best. It's AI agents writing provable handlers in a language designed for exactly this:

```
generate → compile → prove → deploy
```

The tests you used to write to catch unchecked errors, forgotten cases, leaked secrets, and mutated state — the compiler writes the proofs for you. You still write tests for *behavior*. You stop writing tests to *defend against the language*.

## Try It

```bash
zig build run -- examples/handler/handler.ts -p 3000
```

The compiler is the guardrail. The contract is the documentation. The proofs replace the tests you only wrote to appease the language.

And for the AI agent writing your next handler — one obvious way to do everything is the only way that works.
