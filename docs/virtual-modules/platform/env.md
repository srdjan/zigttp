# `zigttp:env`

Environment variable access. Literal-key reads populate the handler
contract; dynamic reads mark the env section as dynamic.

## Summary

```ts
import { env } from "zigttp:env";

function handler(req) {
  const secret = env("JWT_SECRET");
  if (secret === undefined) {
    return Response.text("server misconfigured", { status: 500 });
  }
  return Response.json({ region: env("AWS_REGION") ?? "unknown" });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `env` | `env(name): string \| undefined` | optional string | Read an environment variable. Returns `undefined` when unset. |

`env()` is pure within a request: two reads of the same key return
the same value, so the canonicalizer may collapse repeated calls.

## Compile-time proof

- Every literal `env("NAME")` site adds `NAME` to the handler's
  `env.literal` contract section. With no dynamic reads,
  `env.dynamic: false` lets the runtime sandbox restrict env
  access to exactly the proven names.
- Self-extracting binaries validate every proven env var at
  startup; missing values fail fast instead of surfacing as a 500
  on the first request.
- Return value labeled `secret`, so the flow checker prevents
  leakage into egress or response bodies without an intervening
  sanitization barrier (`mask()`, `escapeHtml()`, etc.).

## Runtime failures

- Never throws. Missing keys return `undefined`.
- The `--no-env-check` server flag disables the startup validation
  of proven env vars. Handlers still work but the fail-fast
  guarantee is waived.

## Related

- [`zigttp:auth`](../security/auth.md) - `jwtVerify(token, env("JWT_SECRET"))`
  is the canonical pairing.
- [`zigttp:text`](./text.md) - `mask()` is the sanitizer for
  accidentally surfacing secrets in logs or responses.
