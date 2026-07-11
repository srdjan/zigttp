# Compile-Time Handler Verification

zigttp can statically prove your handler function is correct at compile time. Enabled via `-Dverify` at build time, zero cost when disabled.

## Usage

```bash
# Build with verification
zig build -Dhandler=handler.ts -Dverify

# Combine with other flags
zig build -Dhandler=handler.ts -Dverify -Daot -Doptimize=ReleaseFast
```

Verification runs after parsing and before bytecode generation. If any error-severity diagnostic is found, the build fails with a non-zero exit code. Warnings are reported but do not fail the build.

## Why This Works

zigttp's JavaScript subset bans most sources of non-trivial control flow:

- No `while`/`do-while` (no back-edges)
- No `try`/`catch` (no exceptional paths)
- No `goto`, no labeled statements

`break` and `continue` are allowed within `for-of` loops. Both are forward jumps only (break jumps past the loop end, continue jumps to the next iteration) and do not introduce back-edges, so the verification invariant holds.

The only control flow is: `if`/`else` (forward branching), `match` (exhaustive forward branching), `for-of` (bounded iteration with `break`/`continue`), `assert` (guard with early return), and `return` (function exit). The IR tree IS the control flow graph. Verification is a recursive tree walk, not a fixpoint dataflow analysis.

## Checks

### 1. Exhaustive Response Returns

Every code path through the handler must return a Response. The verifier recursively determines whether each statement always, never, or sometimes returns:

- `return` - always returns
- `if` without `else` - sometimes (even if the then-branch returns)
- `if`/`else` where both branches return - always returns
- `for-of` - never (iterable could be empty)
- `var_decl`, `expr_stmt` - never returns

**Triggers when:** the handler body does not always return on every code path.

```
verify error: not all code paths return a Response
  --> handler.ts:2:17
   |
  2 | function handler(req) {
   |                 ^
   = help: ensure every branch (if/else, match/default) ends with a return statement
```

**Fix:** add `else` clauses, `match` default arms, or trailing return statements.

### 2. Result Checking

Values from Result-producing virtual module calls (`jwtVerify`, `validateJson`, `validateObject`, `coerceJson`, `decodeJson`, `decodeForm`, `decodeQuery`, `decodeFormMultipart`) must have `.ok` checked before `.value` or `.unwrap()` is accessed.

The verifier tracks result bindings through the control flow tree:

```javascript
import { jwtVerify } from "zigttp:auth";

function handler(req) {
    const token = req.headers.authorization;
    const result = jwtVerify(token, secret, "HS256");

    // ERROR: accessing result.value without checking result.ok
    return Response.json(result.unwrap());
}
```

```
verify error: result.value accessed without checking result.ok first
  --> handler.ts:7:28
   |
  7 |     return Response.json(result.unwrap());
   |                            ^
   = help: check result.ok before accessing result.value:
           if (result.ok) { ... result.value ... }
```

**Recognized patterns for .ok checks:**

- `if (result.ok) { ... }` - direct truthiness
- `if (result.isOk()) { ... }` - method call
- `if (result.ok === true) { ... }` - strict equality
- `if (!result.ok) { return ...; }` - negated early return (code after is safe)
- `if (result.isErr()) { return ...; }` - error early return

### 3. Unreachable Code

Statements after an unconditional return in a block. Severity: warning.

```
verify warning: unreachable code after return statement
  --> handler.ts:4:5
   |
  4 |     const x = 42;
   |     ^
   = help: remove the unreachable code, or restructure the control flow
```

### 4. Unused Variables

Declared variables that are never referenced. Severity: warning. Suppress by prefixing the name with `_`.

The check is scope-aware: a variable used only in a nested scope that shadows an outer declaration does not count as a use of the outer binding.

```
verify warning: unused variable 'temp'
  --> handler.ts:3:11
   |
  3 |     const temp = computeValue();
   |           ^
   = help: remove the variable, or prefix with _ to suppress: _temp
```

### 5. Non-Exhaustive Match

Match expressions without a `default` or `when _` arm are rejected by strict ZigTS unless the type checker can prove every finite union variant is covered.

```
strict error: match expression must be exhaustive in strict ZigTS
  --> handler.ts:5:12
   |
  5 |     return match (req) {
   |            ^
   = help: cover every finite union member or add an explicit default when the type is not finite
```

### 6. Exhaustive Optional Handling

Four virtual module functions return optional values (`T | undefined`):

- `env("KEY")` - returns `string | undefined`
- `cacheGet("ns", "key")` - returns `string | undefined`
- `parseBearer(header)` - returns `string | undefined`
- `routerMatch(routes, req)` - returns `object | undefined`

The verifier tracks these optional bindings and requires them to be narrowed before use. Using an optional value as a function argument, object property value, template literal expression, or in arithmetic/string concatenation without first checking for `undefined` is an error.

```javascript
import { env } from "zigttp:env";

function handler(req) {
    const appName = env("APP_NAME");

    // ERROR: optional value used without checking for undefined
    return Response.json({ app: appName });
}
```

```
verify error: optional value used without checking for undefined
  --> handler.ts:6:30
   |
  6 |     return Response.json({ app: appName });
   |                              ^
   = help: check before use: if (val !== undefined) { ... }
           or provide a default: val ?? "fallback"
```

Property access on an un-narrowed `optional_object` (from `routerMatch`) is also an error:

```
verify error: property access on optional value without checking for undefined
```

**Recognized narrowing patterns:**

- `if (val) { ... }` - truthiness narrows in then-branch
- `if (!val) { return ...; }` - negated early return narrows subsequent code
- `if (val !== undefined) { ... }` - explicit check narrows in then-branch
- `if (val === undefined) { return ...; }` - explicit check with early return
- `const x = env("KEY") ?? "default"` - nullish coalesce resolves at declaration
- `val = "override"` - reassignment to non-optional clears tracking
- `val?.prop` - optional chaining is safe (not flagged)

### 7. State Isolation (Cross-Request Safety)

The verifier detects module-scope variable mutations inside the handler body. Since handlers are re-invoked per request with fresh scope, mutating a module-level `let` binding would leak state between requests.

The check walks all assignment nodes in the handler body. If the target is an identifier whose binding has a scope_id less than the handler's scope_id (meaning it's declared at module level), the verifier emits a `module_scope_mutation` error.

```typescript
// ERROR: handler mutates module-scope variable
let counter = 0;

function handler(req: Request): Response {
    counter += 1;  // verify error: module_scope_mutation
    return Response.json({ count: counter });
}
```

Fix: use `const` for module-level declarations, or move mutable state to `zigttp:cache`.

The result feeds into `HandlerProperties.state_isolated`. When no module-scope mutations are detected, `state_isolated` is proven true, enabling safe multi-tenant handler sharing.

### 8. Author-Declared Spec Discharge

The verifier resolves the handler's active spec set and discharges each
name against the classified `HandlerProperties`. When the handler declares
no `Spec<...>`, every supported v1 spec is active by default. A
`Spec<...>` on the handler return type narrows the active set to exactly
the names in the annotation. The machinery lives in `spec_discharge.zig`
and runs after the analyzer pipeline so it has access to the full property
set plus the imported module list.

```typescript
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"idempotent" | "deterministic">;

function handler(req: Request): Response & Guardrails {
    return Response.json({ now: Date.now() });
}
```

Three diagnostic codes:

- **ZTS500 - spec_not_discharged**: the corresponding property field is
  false. Cause-only specs (`deterministic`, `read_only`, `retry_safe`,
  `idempotent`, `state_isolated`, `fault_covered`, `pure`, `stateless`,
  `result_safe`, `optional_safe`, `cost_bounded`) include a per-property `Try:`
  suggestion. Counterexample-rich specs
  (`no_secret_leakage`, `no_credential_leakage`, `input_validated`,
  `pii_contained`, `injection_safe`) include a falsifying request body.
  `cost_bounded` discharges when the worst-path module-call count has a
  finite envelope; its suggestion points at literal arrays, `range(n)`,
  `.slice(0, k)`, SQL `LIMIT n`, or schema `maxItems`.
- **ZTS501 - spec_incompatible_with_import**: the spec contradicts an
  imported module. v1 fires for `Spec<"read_only">` against
  `zigttp:cache` or `zigttp:sql`. ZTS500 is suppressed for the same
  name so the agent does not enter repair against a contradiction.
- **ZTS502 - spec_unknown_name**: the declared name is not in the v1
  set.

Diagnostics are stored on `HandlerContract.spec_diagnostics`. Surfaces:
the live HUD, the proof studio (failing pills expand inline to the
ZTS5xx code, source line, and snippet), the proof ledger
(`declaredSpecs: [{name, discharged, diagnosticCode?,
diagnosticMessage?, sourceLine?, sourceColumn?, sourceSnippet?}]` per
swap event; the diagnostic fields appear only on failed specs),
`zigts check --json` (`declared_specs` as the effective active set and
`spec_diagnostics` arrays), and the `pi_specs_status` agent tool. See
[user-guide.md](user-guide.md#author-declared-specs) for the author-side
view.

### 9. Proof-Carrying Functions

Spec discharge also runs per helper. A `Proof<T, "...">` annotation on a
helper's return type is discharged against the facts effect inference
and path-return analysis prove about that function (`function_specs.zig`),
for the v1 capsule set `total`, `pure`, `read_only`, `deterministic`.
Helper failures reuse ZTS500 / ZTS502, each carrying a `function`
attribution. A helper that breaks a property the handler's `Spec<...>`
demands while carrying no capsule for it gets **ZTS606 -
missing_capsule**: the proof cannot compose across that call boundary.
`computeProperties` intersects the handler's classified properties with
its call-graph-composed effect row, so a property the handler claims
also accounts for every helper it transitively calls. `zigts check
--json` adds a `proofCapsules` array; see
[zigts-expert-contract.md](internals/zigts-expert-contract.md).

Counterexample-rich specs additionally feed a persistent on-disk
corpus. Each falsifying input the analyzer materialises is written
under `.zigttp/witnesses/<short_hash>/` so the same logical leak does
not need to be rediscovered next session. See [witnesses.md](witnesses.md)
for layout, CLI (`zigttp witnesses`), and agent tool (`pi_witnesses`).

### 10. Capability Capsules

`Effects<T, "...">` is the capability dual of `Proof<T, "...">`. Where a
proof property declares a guarantee, an `Effects<...>` annotation
declares a *ceiling*: the function's inferred capability row may be no
wider than the named set. Discharge is the inverse direction of proof
discharge - checked `inferred ⊆ declared` against the row
`effect_inference.zig` computes from real call sites. A reached
capability outside the ceiling is **ZTS503**, an unknown capability name
is **ZTS504**, and a declared-but-unreached capability is the warning
**ZTS505**. The vocabulary is the runtime capability set (`env`,
`clock`, `crypto`, `network`, ...).

The same annotation on the handler's return type is a **budget** that
bounds every reachable helper. A capability the handler reaches directly
outside the budget is **ZTS506**; one a reachable helper introduces is
**ZTS607**, attributed to that helper by `contract_builder.zig`.
The declared budget is recorded in `contract.json` under
`sandbox.declaredBudget`. `zigts check --json` adds an `effectCapsules`
array alongside `proofCapsules`.

`Effects<...>` is opt-in - a function with no annotation gets no check.
The budget and every ceiling are discharged only against inferred facts
from real function bodies, never an assumed claim. The opt-in
`zigts check --require-export-capsules` docs mode additionally warns
(**ZTS507** / **ZTS508**) when an exported helper carries no capsule.

### Runtime Optimizations from Verification

Verified properties also control runtime behavior:

- **Route pre-filtering**: proven routes reject non-matching requests at the HTTP layer before entering JS (`contract_runtime.zig`).
- **Response memoization**: when a handler is proven `deterministic` (no Date.now or Math.random) and `read_only` (no write-classified virtual module calls), and its contract shows it reads no request headers or body, GET/HEAD responses are cached in memory and served without JS execution. The header/body condition is required because the cache key is method+URL only: a handler whose response varies on a request header (auth, content negotiation) is excluded so one caller's response is never replayed to another. Cached responses include an `X-Zigttp-Proof-Cache: hit` header (`proof_adapter.zig`).

Both rely on the same property that makes verification tractable: the IR tree is the control flow graph, with no back-edges and no exceptions.

## Exhaustive Path Analysis (-Dgenerate-tests)

The `-Dgenerate-tests=true` build option enables compile-time exhaustive path enumeration and fault coverage analysis.

### Path Generator

The `PathGenerator` (`packages/zigts/src/path_generator.zig`) walks the handler's IR tree, forking at every branch point (`if`/`match`) and I/O success/failure boundary. Each fork produces a test case representing one complete execution path through the handler. This exploits the same property that makes verification tractable: the IR tree IS the control flow graph (no back-edges).

For Result-producing calls (`jwtVerify`, `validateJson`, `decodeJson`, etc.), the generator forks into both the `ok: true` and `ok: false` paths. For optional-producing calls (`env`, `cacheGet`, etc.), it forks into defined and `undefined` paths.

### Fault Coverage Analysis

The `FaultCoverageChecker` (`packages/zigts/src/fault_coverage.zig`) analyzes the generated paths against `FailureSeverity` annotations on each virtual module function:

| Severity | Functions | Meaning |
|----------|-----------|---------|
| `critical` | `jwtVerify`, `validateJson`, `validateObject`, `coerceJson`, `decodeJson`, `decodeForm`, `decodeQuery`, `decodeFormMultipart` | Auth/validation failures must not produce 2xx |
| `expected` | `cacheGet`, `env` | Cache misses and missing env vars are normal degradation |

The checker warns when a critical I/O failure path (e.g., `jwtVerify` returning `{ok: false}`) produces a 2xx response. This is a structural pattern overwhelmingly correlated with bugs. Cache misses returning 200 are correctly classified as graceful degradation and do not trigger warnings.

Results appear in:
- `contract.json` under the `faultCoverage` section
- `HandlerProperties.fault_covered` flag
- Deployment manifest tags (`zigttp:faultCovered`)
- Build report alongside other PROVEN/--- labels

## Running Tests

```bash
bash tests/verify/run_tests.sh
```

The test suite verifies expected diagnostics from handler files in `tests/verify/`.
