# TypeScript Patterns In The zts Subset

The community list ["TypeScript Tips Everyone Should
Know"](https://github.com/AllThingsSmitty/typescript-tips-everyone-should-know)
is good advice for plain TypeScript. This document maps that canon onto the
zts subset and shows what changes when the advice meets a compiler that
enforces it.

The lens is simple. The subset turns several tips from advice into compiler
guarantees: `any` and `enum` are hard errors, `null` is gone (the single
absent-value sentinel is `undefined`), and `match` does exhaustiveness checking
natively. It adapts a few tips because the underlying construct works
differently here: zts rejects both `as` and `satisfies`, so a conformance
check lives on the declaration instead. And it leaves one construct out by
design: `typeof`-type extraction would derive a type from a value, the inverse
of the contract-first direction the rest of this runtime depends on.

For the full allow and reject list see
[Feature Detection](feature-detection.md). For why each cut exists and what
proof it buys, see [Restrictions to Proofs](restrictions-to-proofs.md). This
document assumes the type-system mechanics covered in
[TypeScript](typescript.md).

## Mapping Table

| # | Canon tip | Verdict | zts idiom |
|---|-----------|---------|-------------|
| 1 | Prefer `unknown` over `any` | Enforced | `any` is a hard error; `unknown` plus a type guard is the only path -> [unknown-and-guards.ts](../examples/patterns/unknown-and-guards.ts) |
| 2 | Let type inference do the work | Direct fit | annotate boundaries, infer internals -> [infer-and-generics.ts](../examples/patterns/infer-and-generics.ts) |
| 3 | Prefer `satisfies` over `as` | Adapted | both rejected; annotate the declaration -> [annotate-not-assert.ts](../examples/patterns/annotate-not-assert.ts) |
| 4 | Derive types from values | Partial | `typeof`-extraction absent by design; declare the alias, `const` keeps literals -> [annotate-not-assert.ts](../examples/patterns/annotate-not-assert.ts) |
| 5 | Discriminated unions for impossible states | Direct fit (core) | tagged unions plus `match` narrowing; `undefined`-only sentinel -> [discriminated-union-match.ts](../examples/patterns/discriminated-union-match.ts) |
| 6 | Exhaustive checks with `never` | Adapted (native) | `match` exhaustiveness is native with a required `default` arm; the `never`-helper is unneeded -> [discriminated-union-match.ts](../examples/patterns/discriminated-union-match.ts) |
| 7 | `as const` for config and constants | Adapted | `const` bindings preserve literals automatically; no `as const` -> [literal-types-no-enum.ts](../examples/patterns/literal-types-no-enum.ts) |
| 8 | Type predicates for reusable narrowing | Direct fit | `x is T` guards; pairs with `assert` -> [unknown-and-guards.ts](../examples/patterns/unknown-and-guards.ts) |
| 9 | Build new types from existing (`Pick`/`Omit`/`Partial`) | Direct fit | `Pick`/`Omit`/`Partial`/`Required` derive shapes from a source type, joining `Readonly<T>` -> [derive-types.ts](../examples/patterns/derive-types.ts) |
| 10 | Validate external data at runtime | Direct fit (built-in) | `zttp:validate` plus `zttp:decode` replace Zod -> [validate-external.ts](../examples/patterns/validate-external.ts) |
| 11 | Avoid `enum` | Enforced | `enum` is a hard error; literal unions only -> [literal-types-no-enum.ts](../examples/patterns/literal-types-no-enum.ts) |
| 12 | Generics that infer automatically | Direct fit | generics plus inference (up to 8 params) -> [infer-and-generics.ts](../examples/patterns/infer-and-generics.ts) |
| 13 | Strict compiler options | Adapted | no tsconfig; sound mode plus the canonical profile plus the analyzer are strict by construction ([sound-mode.md](sound-mode.md), [canonical-profile.md](canonical-profile.md)) |
| 14 | Template literal types | Direct fit | template literal types supported -> [literal-types-no-enum.ts](../examples/patterns/literal-types-no-enum.ts) |
| 15 | Type-safe is not runtime-safe | Direct fit (thesis) | proof receipts, contracts, and runtime validation; the restrictions-to-proofs story ([restrictions-to-proofs.md](restrictions-to-proofs.md), the proof-receipt section of [user-guide.md](user-guide.md)) |

A note on the examples. Every tip with a code companion links to a handler under
`examples/patterns/`, and all seven are on disk and compile: each passes
`zttp check --types`, and three (`validate-external`,
`discriminated-union-match`, and `derive-types`) also run as behavioral
suites under `scripts/test-examples.sh`. A few snippets
below are shortened excerpts of those files, or of the matching sections in
[TypeScript](typescript.md) and [User Guide](user-guide.md) that exercise the
same constructs. Every snippet in this document compiles in the subset.

## Enforced

These tips are not advice in zts. The compiler rejects the alternative.

### 1. Prefer `unknown` over `any`

Idiom: `any` is a hard error, so a value of unknown shape is typed `unknown` and
narrowed with a reusable `x is T` predicate before use.

The stripper rejects the `any` type outright (see the unsupported-features
table in [TypeScript](typescript.md)). That removes the escape hatch the canon
warns about: there is no `any` to drift into. The only way to use a value of
unknown shape is to narrow it. A type-guard function plus an `assert` statement
installs that narrowing:

```typescript
function isString(x: unknown): x is string {
    return typeof x === "string";
}

assert isString(val);
val.toUpperCase();       // narrowed from here forward
```

See [unknown-and-guards.ts](../examples/patterns/unknown-and-guards.ts) and the
"Type Guards and Assert" section of [TypeScript](typescript.md).

### 11. Avoid `enum`

Idiom: `enum` is a hard error; model the finite set as a string literal union.

The canon lists "avoid enum" as a discipline. zts makes it a parse error: a
finite set of values is a union of string literals joined with `|`, which the
compiler can check for exhaustiveness in a `match`.

```typescript
type Method = "GET" | "POST" | "DELETE";

const defaultMethod: Method = "GET";
```

Excerpted from [literal-types-no-enum.ts](../examples/patterns/literal-types-no-enum.ts).
The `enum` restriction and the failure class it removes are in
[Restrictions to Proofs](restrictions-to-proofs.md).

## Direct Fit

These tips translate one-to-one. The construct exists and behaves as the canon
describes.

### 2. Let type inference do the work

Idiom: annotate the boundaries (parameters and return), let inference handle the
internals.

zts requires explicit parameter and return annotations on named functions
(strict ZigTS), which is exactly the canon's "annotate the boundary" advice made
mandatory. Inside the body, locals infer from their initializers, so redundant
local annotations are unnecessary.
See [infer-and-generics.ts](../examples/patterns/infer-and-generics.ts).

### 5. Discriminated unions for impossible states

Idiom: tag each variant, dispatch with `match`, and let the tag make impossible
states unrepresentable. There is one absent-value sentinel, `undefined`, never
`null`.

```typescript
type Result = { kind: "ok", value: string } | { kind: "err", error: string };

if (r.kind === "err") {
    return Response.json({ error: r.error }, { status: 400 });
}
// r is narrowed to { kind: "ok", value: string } from here
r.value.toUpperCase();
```

This is the core idiom of the subset, not an add-on: `null` is removed entirely
(see [Restrictions to Proofs](restrictions-to-proofs.md)), so a discriminated
union is the single way to carry "either this or that." Excerpted from the
"Discriminated Union Narrowing" section of [TypeScript](typescript.md); the
companion example is
[discriminated-union-match.ts](../examples/patterns/discriminated-union-match.ts).

### 8. Type predicates for reusable narrowing

Idiom: a `x is T` predicate is a reusable narrowing function; it pairs with
`assert` for forward narrowing or with `if` for branch narrowing.

```typescript
function isString(x: unknown): x is string {
    return typeof x === "string";
}

if (isString(val)) {
    val.toUpperCase();   // narrowed in then-branch
}
```

The same predicate drives tip 1. See the "Type Guards and Assert" section of
[TypeScript](typescript.md) and
[unknown-and-guards.ts](../examples/patterns/unknown-and-guards.ts).

### 10. Validate external data at runtime

Idiom: there is no Zod step. Compile a schema by name at the top level with
`zttp:validate`, then gate the handler on the `.ok` of the result before
touching `.value`.

```typescript
import { schemaCompile, validateJson } from "zttp:validate";

schemaCompile("todo", '{"type":"object","required":["title"]}');

function handler(req: Request): Response {
    const parsed = validateJson("todo", req.body);
    if (!parsed.ok) {
        return Response.json({ error: "invalid body" }, { status: 400 });
    }
    return Response.json(parsed.value, { status: 201 });
}
```

The verifier enforces the `.ok`-before-`.value` discipline at build time:
`validateJson` is a Result-producing call, so accessing `.value` on an
unchecked result is a compile error (see [Verification](verification.md)). The
built-in modules `zttp:validate` and `zttp:decode` cover the runtime
validation the canon reaches for a library to do. Excerpted from the "JSON And
Validation" section of [User Guide](user-guide.md); the companion example is
[validate-external.ts](../examples/patterns/validate-external.ts).

Testing note: `zttp:validate` and `zttp:decode` are pure (their result is a
function of their arguments and the compiled schema), so a handler test under
`serve --test` runs them for real without an `io` mock. The
`validate-external.test.jsonl` suite posts a body and asserts the 201/400 path
against the real validator. Effectful modules (`fetch`, `cache`, `sql`, `env`,
random-backed `id`) still need recorded `io` entries to stay deterministic.

### 12. Generics that infer automatically

Idiom: write one generic helper and reuse it across element types by naming the
type argument at the call site.

A helper such as `first<T>(xs: T[]): T | undefined` is reused as
`first<string>(items)`, and the result type flows into the handler. zts
supports generic function declarations and generic arrow functions, with up to 8
type parameters per alias (see "Generic Type Aliases" in
[TypeScript](typescript.md)). The companion example is
[infer-and-generics.ts](../examples/patterns/infer-and-generics.ts).

### 14. Template literal types

Idiom: constrain a string to a pattern with a template literal type, checked at
build time.

```typescript
type ApiRoute = `/api/${string}`;

const defaultRoute: ApiRoute = "/api/health";
```

A value that does not match the pattern is a compile error (the canon's
intent, enforced). Excerpted from
[literal-types-no-enum.ts](../examples/patterns/literal-types-no-enum.ts); see
also "Template Literal Types" in [TypeScript](typescript.md).

### 9. Build new types from existing (`Pick`/`Omit`/`Partial`/`Required`)

Idiom: the utility-type family derives a related shape from one source type, so
each field stays declared in a single place.

`Pick<T, Keys>` keeps only the named fields, `Omit<T, Keys>` drops them,
`Partial<T>` makes every field optional, and `Required<T>` makes every field
required. They resolve structurally and work against a named source type, not
just an inline object, joining the already-supported `Readonly<T>` (see
"Readonly Fields" in [TypeScript](typescript.md)).

```typescript
type User = { id: number; name: string; email: string; age: number };
type Summary = Pick<User, "id" | "name">; // { id: number; name: string }
type Safe = Omit<User, "email">;          // id, name, age
type UserPatch = Partial<User>;           // every field optional
```

See [derive-types.ts](../examples/patterns/derive-types.ts). Intersection (`&`)
still composes narrower types where a utility does not fit:
`type WithMeta = Base & { createdAt: string }`.

## Adapted

These tips map to a different construct, because the canonical TypeScript
mechanism is absent or replaced.

### 3. Prefer `satisfies` over `as`

Idiom: zts rejects both `as` and `satisfies`, so the conformance check moves
onto the declaration. An explicit annotation on a `const` binding is the
assertion-free `satisfies`.

The canon recommends `satisfies` because it checks conformance without widening
the type. zts removes the question by rejecting both assertion forms. The
replacement is to annotate the binding directly: `const config: Config = {...}`
checks the literal against `Config` and keeps its narrow type, which is what
`satisfies` was for.

```typescript
type Config = { port: number; host: string; readonly version: string };
const config: Config = { port: 8080, host: "0.0.0.0", version: "1.0" };
```

See [annotate-not-assert.ts](../examples/patterns/annotate-not-assert.ts). The
rejection of `as` and `satisfies` is covered in the unsupported-features table
of [TypeScript](typescript.md).

### 4. Derive types from values

Idiom: `typeof`-type extraction is left out by design, so the type alias is the
declared contract the value is checked against; a `const` binding keeps its
literal type from the annotation without `as const`.

In plain TypeScript you can write `type Config = typeof config` to derive a type
from a value. That direction is not available here, and its absence is the
point: the alias is declared first and the value is checked against it, so the
type stays an independent contract. A type derived from the value it describes
would have nothing left to check. The same
[annotate-not-assert.ts](../examples/patterns/annotate-not-assert.ts) example
shows the declared-alias form, and the rationale is under Deliberately Absent
below.

### 6. Exhaustive checks with `never`

Idiom: `match` exhaustiveness is native, so the `never`-typed `assertNever`
trick is unnecessary.

In plain TypeScript the standard exhaustiveness guard is a `default` branch that
assigns the discriminant to a `never`-typed parameter (`assertNever(x: never)`),
which stops compiling when a new variant is added. zts checks `match`
exhaustiveness directly, and the canonical profile requires every `match` to
carry a `default` / `when _:` catch-all arm so the unexpected case is always
handled. Full variant coverage satisfies the type-level exhaustiveness check but
does not lift that requirement - it applies to a local discriminant just as much
as a parameter. So the rule is simple: give every `match` a `default` arm, and
the `never`-parameter helper buys nothing. The companion
[discriminated-union-match.ts](../examples/patterns/discriminated-union-match.ts)
dispatches on a parameter, carries a `default` arm, and checks with every
property proven and no warnings.

### 7. `as const` for config and constants

Idiom: a `const` binding preserves its literal type from the annotation on its
own. No `as const`.

```typescript
// No `as const` here. The annotation pins the literal type on its own.
const defaultMethod: Method = "GET";
const defaultRoute: ApiRoute = "/api/health";
```

`const x = 200` already has type `200`, not `number` (see "Literal Types and
Annotation Semantics" in [TypeScript](typescript.md)), so the `as const`
assertion has nothing to add and is rejected along with the other assertions.
Excerpted from
[literal-types-no-enum.ts](../examples/patterns/literal-types-no-enum.ts).

### 13. Strict compiler options

This is a philosophy tip, so there is no snippet. zts has no `tsconfig.json`
and no opt-in strictness dial. Strict ZigTS is the default profile: `any` is
rejected, named functions must carry parameter and return annotations,
capability access must use literal keys, and an avoidable `let` is an error.
Layered on top are sound mode (type-directed truthiness, arithmetic, and
comparison diagnostics) and the canonical profile, both strict by construction
rather than by flag. See [Sound Mode](sound-mode.md) and
[Canonical Profile](canonical-profile.md). The canon's "turn on every strict
option" reduces to "the strict options are the only options."

## Type-Safe Is Not Runtime-Safe (Tip 15)

The closing tip of the canon is the thesis of this whole runtime, so it gets
prose rather than a snippet. A passing type check proves shape, not behavior at
the boundary. zts answers that on two fronts. At the boundary, runtime
validation through `zttp:validate` and `zttp:decode` (tip 10) checks the
data that types alone cannot. Above the type system, the
restrictions-to-proofs story turns each language cut into a discharged property
(exhaustive returns, state isolation, no secret leakage, and the rest), and
every build can sign those properties into a proof receipt that a third party
verifies. See [Restrictions to Proofs](restrictions-to-proofs.md) for the
cut-to-proof table and the proof-receipt section of
[User Guide](user-guide.md) for the signed receipts.

## Deliberately Absent

Two constructs a TypeScript author might reach for are missing, and both
absences are choices rather than unfinished work. They are listed here so the
omission reads as deliberate.

`typeof`-type extraction (`type Config = typeof config`) derives a type from a
value. That inverts the relationship the rest of this runtime depends on: a type
is the independent contract a value is checked against, and a contract derived
from the value it describes cannot be violated by that value. The declared-alias
direction (tip 4) keeps the type as the spec and the value as the thing measured
against it, which is the whole point of a checker.

An assertion-form `satisfies` is the operator the subset removed on purpose. `as`
and `satisfies` are both assertions, and rejecting them is what lets an
annotated `const` stand as the single source of truth (tip 3). A `satisfies`
that checks without widening would hand back the narrower-than-declared shape:
the second, drifting type the rejection was meant to eliminate. The ergonomic
saving is real and small; the assertion-free guarantee is the larger thing, and
not worth trading for it.
