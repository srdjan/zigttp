# Canonical ZigTS Profile

ZigTS already trims most of TypeScript. The canonical profile trims further: for every operation there is exactly one canonical spelling, and the compiler rejects the alternates as ZTS6xx diagnostics. This page is the reference for each canonical rule, its rationale, and how to migrate code that violates it.

The profile is enforced unconditionally on every `zigttp check` and `zigttp verify-paths` run; there is no opt-in flag. Existing code that does not yet conform should be migrated rule by rule.

## Why narrow the grammar further

A smaller grammar carries three compounding wins:

1. **Compiler proofs become cheaper.** Fewer expression shapes means fewer cases in every walker, the type checker, and the contract extractor. The proof card becomes proportionally easier to keep accurate.
2. **Agent reliability rises.** `zigttp expert` generates code from a much smaller legal output space, which means fewer "valid TypeScript" idioms compete for the same role. Mis-generations drop because there are simply fewer alternates to pick.
3. **Review becomes mechanical.** When every operation has one spelling, a diff that adds a banned form is a structural signal rather than a stylistic preference. Veto rejections become unambiguous.

See `docs/restrictions-to-proofs.md` for the broader table that maps each language cut to the failure class it eliminates and the proof it unlocks.

## Rule reference

Each rule lists the diagnostic code, the canonical replacement, the rationale, and a migration recipe. All rules emit at error severity through `strict_checker.zig`.

### ZTS604 - avoidable `let`

```ts
// before
let region = env("REGION") ?? "iad";

// after
const region = env("REGION") ?? "iad";
```

A `let` binding that is never reassigned should be `const`. The strict checker treats reassignment as the trigger to keep `let`; in its absence, the canonical form is `const`. Why: block-scoped immutability lets the data-flow analysis treat the binding as a value, which strengthens several derived proofs.

### ZTS605 - dynamic computed property access

```ts
// before
return obj[req.headers.get("key") ?? "default"];

// after
const key = "userName";
return obj[key];
// or, better, use a typed field: obj.userName
```

Dynamic indexing hides object shape. The canonical form uses a static literal or a `const`-bound literal.

### ZTS608 - reused arrow helper

```ts
// before
const parse = (x: number): number => x;

// after
function parse(x: number): number {
  return x;
}
```

Arrow functions are reserved for callbacks passed directly as values. Anything reused as a helper is a named `function` declaration. Why: named functions show up in stack traces, can be referenced by tools, and signal "reused helper" to readers.

### ZTS609 - exported function-valued `const`

```ts
// before
export const handler = (req: Request): Response => Response.json({ok: true});

// after
export function handler(req: Request): Response {
  return Response.json({ok: true});
}
```

A public API that is a function uses `export function`; `export const` is reserved for non-function values. Why: the public surface should make "this is a function" obvious at the binding site, not behind an arrow on the right-hand side.

### ZTS610 / ZTS611 - public helper effects / proof capsules

```ts
// before
export function loadUser(id: string): User { return sqlOne("getUser", {id}); }

// after
export function loadUser(id: string): Effects<User, "sql.read"> {
  return sqlOne("getUser", {id});
}
```

Public helpers that touch capabilities must declare an `Effects<...>` ceiling; helpers used under declared proofs must carry `Proof<...>`. Why: the contract extractor reads these capsules; missing them means the proof card cannot reach the helper.

### ZTS612 - ternary expression

```ts
// before
const status = ok ? 200 : 500;

// after
let status: number;
if (ok) {
  status = 200;
} else {
  status = 500;
}
```

Ternary expressions compete with `if`/`else` and `match` for the same role and tend to nest in agent-generated code. The canonical form uses statement-level branching. For closed alternatives prefer `match`; for guard-style branching use `if`/`else`. When the result feeds a single binding, a tail `if`/`else` chain or an immediately-invoked function expression keeps the intent local.

### ZTS613 - compound assignment

```ts
// before
count += 1;
total -= delta;
average *= 1.05;

// after
count = count + 1;
total = total - delta;
average = average * 1.05;
```

Compound assignment operators (`+=`, `-=`, `*=`, `/=`, `%=`, `**=`) are banned alongside the existing prohibition on `++` and `--`. Why: one explicit form per arithmetic update. The expanded form makes the right-hand-side expression diff-visible.

### ZTS614 - non-leading object spread

```ts
// before
const next = {status: "ok", ...base};

// after
const next = {...base, status: "ok"};
```

When an object literal mixes a spread and explicit keys, the spread must come first. Later keys still override; the leading-spread form makes "base then overrides" the only legal reading. Why: kills the "which keys win" footgun without losing the immutable-update idiom.

### ZTS615 - complex template interpolation

```ts
// before
return Response.text(`user ${getUser().name} at ${Date.now()}`);

// after
const user = getUser();
const now = Date.now();
return Response.text(`user ${user.name} at ${now}`);
```

Template interpolations must be identifiers or chains of literal-keyed member access. Function calls, arithmetic, and ternaries must be hoisted into an intermediate `const`. Why: the intermediate names show up in reviews and make the values inspectable in trace output.

### ZTS616 - call-site spread

```ts
// before
return send(...args);

// after
return send(args[0], args[1], args[2]);
// or widen the helper: function send(args: SendArgs): Response
```

Spread at the call site is banned; spread in object literals is fine. Why: arity becomes dynamic and the contract extractor cannot reason about the helper's parameter shape. (Parser support for `f(...args)` is currently incomplete on a separate code path; the strict-checker rule is in place for when that lands.)

### ZTS617 - default parameter value

```ts
// before
function greet(name: string = "world"): string {
  return `hello ${name}`;
}

// after
function greet(name: string | undefined): string {
  const resolved = name === undefined ? "world" : name;
  return `hello ${resolved}`;
}
```

Defaults at the signature site are invisible to callers and to the contract extractor. The canonical form makes the default a visible statement in the body and forces every caller to pass either a value or `undefined`. Why: defaults become diff-visible and the proof card sees them.

### ZTS618 - nested destructuring

```ts
// before
const {user: {name}} = payload;
const [first, [nested]] = pairs;

// after
const {user} = payload;
const {name} = user;
const [first, second] = pairs;
const [nested] = second;
```

Destructuring patterns must be at most one level deep. Drill into nested values with follow-up `const` bindings. Why: the intermediate names show up in errors, panics, and trace output; deeply nested patterns inflate review cost and are a common drift target in agent output.

### ZTS619 - unused index alias in `for...of`

```ts
// before
for (const pair of items.entries()) {
  const [_i, item] = pair;
  use(item);
}

// after
for (const item of items) {
  use(item);
}
```

A `for (const pair of arr.entries())` whose body destructures `[_i, item]` and never reads `_i` carries the `.entries()` call and the alias binding for no payoff. The canonical form drops both and iterates the array directly. Why: the unused index widens the loop's surface for iterator-scope-confinement analysis - the analyzer must track an extra binding only to prove it dead. Eliminating the alias collapses that branch and the proof goes through without an alias-tracking pass.

## Deferred cuts

Two cuts from the original plan remain unimplemented because they require infrastructure the strict checker does not currently carry:

- **Truthy `||` fallback** would require non-null type-checker access in the strict-checker init path to distinguish `boolean || boolean` from `string || string`. The rule is sound only with type information; a structural-only version would over-fire on legitimate boolean expressions.
- **Optional-parameter syntax `(a?: T)`** would require detection at the stripper level, because the `?` marker is removed before the parser runs. The stripper has its own diagnostic vocabulary; wiring it into the strict-checker rule registry is out of scope for this round.

Both will land when their respective infrastructure is in place.

## How the agent learns the profile

`zigttp expert` consumes this profile through two paths:

1. The `canonical-style` skill (`packages/pi/src/skills/canonical-style.md`) is embedded into the persona on every prompt assembly. It lists every canonical form with before/after pairs.
2. Live rule snapshots ride into the system prompt via `zigts_expert_describe_rule` and `zigts_expert_features`. The agent calls these tools rather than answering from memory, so any future rule change reaches the agent the next time a session starts.

Every proposed edit passes through the existing veto (`packages/pi/src/veto.zig`), which rejects any patch that introduces new diagnostics. Canonical violations are diagnostics like any other, so the veto blocks them automatically.

## Verification

```bash
zig build test-zigts                              # unit tests for each detector
zig build test                                    # full test suite + golden fixtures
zigttp describe-rule --hash                       # current policy hash
zigttp check --json examples/handler/handler.ts   # canonical lane on a known-clean handler
```

When the policy hash changes (every time a rule is added, retired, or moved between categories), the golden fixtures under `packages/tools/tests/fixtures/expert/` need a one-line bump alongside the source change.
