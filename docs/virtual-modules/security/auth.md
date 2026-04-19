# `zigttp:auth`

JWT signing and verification, Bearer token parsing, webhook signature
checks, constant-time comparison.

## Summary

```ts
import { parseBearer, jwtVerify, jwtSign } from "zigttp:auth";

function handler(req) {
  const token = parseBearer(req.headers["authorization"]);
  if (token === undefined) {
    return Response.json({ error: "unauthorized" }, { status: 401 });
  }

  const auth = jwtVerify(token, env("JWT_SECRET") ?? "");
  if (!auth.ok) {
    return Response.json({ error: auth.error }, { status: 401 });
  }

  return Response.json({ user: auth.value });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `parseBearer` | `parseBearer(header): string \| undefined` | optional string | Extract the token from an `Authorization: Bearer ...` header. |
| `jwtVerify` | `jwtVerify(token, secret, options?): Result<Claims>` | Result | Verify HS256 signature and `exp` / `nbf` claims. |
| `jwtSign` | `jwtSign(claims, secret): string` | string | Sign an HS256 JWT. |
| `verifyWebhookSignature` | `verifyWebhookSignature(body, signature, secret): boolean` | boolean | HMAC-SHA256 check for a webhook payload. |
| `timingSafeEqual` | `timingSafeEqual(a, b): boolean` | boolean | Constant-time string comparison. |

`Claims` is the decoded JSON payload, treated as an unknown record.
Callers narrow with runtime checks or `decodeJson` before use.

## Compile-time proof

- `jwtVerify()` sets the `bearer_auth` contract flag; proven properties
  for the handler include `authenticated` when every response branch
  passes through a successful verify.
- Labels the returned value as `credential` + `validated` so the flow
  checker can prevent credential leakage into egress.

## Runtime failures

- `jwtVerify()` returns `{ ok: false, error }` on bad signature,
  expired token, or malformed JWT. Check `.ok` before `.value`.
- `jwtSign()` throws synchronously on non-string claims or secrets.

## Requirements

- A non-empty secret. Reading the secret from `env()` is the
  idiomatic path; sound mode rejects `jwtVerify(token, "")` as a
  critical failure because the absorbing law folds it to
  `result_err`.

## Related

- [`zigttp:crypto`](./crypto.md) - lower-level HMAC / SHA256 / base64.
- [`zigttp:validate`](./validate.md) - schema-backed validation for
  claim payloads once decoded.
