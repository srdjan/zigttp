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

zigttp's JavaScript subset bans all sources of non-trivial control flow:

- No `while`/`do-while` (no back-edges)
- No `break`/`continue` (no non-local jumps)
- No `try`/`catch` (no exceptional paths)
- No `goto`, no labeled statements

The only control flow is: `if`/`else` (forward branching), `switch`/`case` (forward branching), `for-of` (bounded iteration), and `return` (function exit). The IR tree IS the control flow graph. Verification is a recursive tree walk, not a fixpoint dataflow analysis.

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

## Running Tests

```bash
bash tests/verify/run_tests.sh
```

The test suite verifies expected diagnostics from handler files in `tests/verify/`.
