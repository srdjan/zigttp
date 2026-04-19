# `zigttp:crypto`

SHA-256 hashing, HMAC-SHA256, and base64 encode/decode. Pure functions
with no side effects.

## Summary

```ts
import { sha256, hmacSha256, base64Encode } from "zigttp:crypto";

function handler(req) {
  const digest = sha256("hello");
  const signature = hmacSha256("secret", req.body);
  const encoded = base64Encode(signature);
  return Response.text(encoded);
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `sha256` | `sha256(input): string` | hex string | SHA-256 digest. |
| `hmacSha256` | `hmacSha256(key, message): string` | hex string | HMAC-SHA256 signature. |
| `base64Encode` | `base64Encode(input): string` | string | Standard base64 encode. |
| `base64Decode` | `base64Decode(input): string` | string | Standard base64 decode (throws on invalid input). |

All functions are pure (same input always returns the same output).
`base64Encode` and `base64Decode` are declared as inverse pairs, so
the optimizer may collapse `base64Decode(base64Encode(x))` to `x`.

## Compile-time proof

- Pure laws allow the canonicalizer to dedupe repeated calls with
  literal arguments.
- No contract extractions; crypto calls don't declare egress,
  env vars, or routes.

## Runtime failures

- `base64Decode()` throws on malformed input. `sha256` and
  `hmacSha256` accept any string input without validation.
- Non-string arguments throw synchronously with a TypeError.

## Related

- [`zigttp:auth`](./auth.md) - higher-level JWT and webhook
  verification built on top of these primitives.
- [`zigttp:id`](../platform/id.md) - ID generators that use the
  platform's random source.
