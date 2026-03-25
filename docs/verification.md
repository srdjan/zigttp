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

The only control flow is: `if`/`else` (forward branching), `switch`/`case` (forward branching), `for-of` (bounded iteration with `break`/`continue`), and `return` (function exit). The IR tree IS the control flow graph. Verification is a recursive tree walk, not a fixpoint dataflow analysis.

## Checks

### 1. Exhaustive Response Returns

Every code path through the handler must return a Response. The verifier recursively determines whether each statement always, never, or sometimes returns:

- `return` - always returns
- `if` without `else` - sometimes (even if the then-branch returns)
- `if`/`else` where both branches return - always returns
- `switch` with `default` and all cases returning - always returns
- `for-of` - never (iterable could be empty)
- `var_decl`, `expr_stmt` - never returns

**Triggers when:** the handler body does not always return on every code path.

```
verify error: not all code paths return a Response
  --> handler.ts:2:17
   |
  2 | function handler(req) {
   |                 ^
   = help: ensure every branch (if/else, switch/default) ends with a return statement
```

**Fix:** add `else` clauses, `default` cases, or trailing return statements.

### 2. Result Checking

Values from Result-producing virtual module calls (`jwtVerify`, `validateJson`, `validateObject`, `coerceJson`) must have `.ok` checked before `.value` or `.unwrap()` is accessed.

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

Match expressions without a `default` arm. Severity: warning.

```
verify warning: match expression has no default arm
  --> handler.ts:5:12
   |
  5 |     return match (req) {
   |            ^
   = help: add a default arm: default: Response.text("Not Found", { status: 404 })
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

## Exhaustive Path Analysis (-Dgenerate-tests)

The `-Dgenerate-tests=true` build option enables compile-time exhaustive path enumeration and fault coverage analysis.

### Path Generator

The `PathGenerator` (`zts/path_generator.zig`) walks the handler's IR tree, forking at every branch point (`if`/`match`/`switch`) and I/O success/failure boundary. Each fork produces a test case representing one complete execution path through the handler. This exploits the same property that makes verification tractable: the IR tree IS the control flow graph (no back-edges).

For Result-producing calls (`jwtVerify`, `validateJson`, etc.), the generator forks into both the `ok: true` and `ok: false` paths. For optional-producing calls (`env`, `cacheGet`, etc.), it forks into defined and `undefined` paths.

### Fault Coverage Analysis

The `FaultCoverageChecker` (`zts/fault_coverage.zig`) analyzes the generated paths against `FailureSeverity` annotations on each virtual module function:

| Severity | Functions | Meaning |
|----------|-----------|---------|
| `critical` | `jwtVerify`, `validateJson`, `validateObject`, `coerceJson` | Auth/validation failures must not produce 2xx |
| `expected` | `cacheGet`, `env` | Cache misses and missing env vars are normal degradation |
| `upstream` | `fetchSync` | Upstream failures should be surfaced, not swallowed |

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
