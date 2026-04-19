# `zigttp:url`

URL parsing, query string handling, percent-encoding. Pure functions
mirrored from the WHATWG URL spec but scoped to the primitives
handlers actually need.

## Summary

```ts
import { urlParse, urlSearchParams, urlEncode, urlDecode } from "zigttp:url";

function handler(req) {
  const parsed = urlParse(req.url);
  const query = urlSearchParams(parsed.search);

  const next = query["next"] ?? "/";
  const safe = urlEncode(next);

  return Response.json({
    origin: parsed.origin,
    path: parsed.pathname,
    encodedNext: safe,
    decodedNext: urlDecode(safe),
  });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `urlParse` | `urlParse(input): { protocol, host, hostname, port, pathname, search, hash, origin }` | object | Parse a URL string. |
| `urlSearchParams` | `urlSearchParams(search): Record<string,string>` | object | Parse a query string (with or without leading `?`) into a plain object. |
| `urlEncode` | `urlEncode(input): string` | string | Percent-encode a string for use in a URL path or query value. |
| `urlDecode` | `urlDecode(input): string` | string | Decode a percent-encoded string. |

All four are pure. `urlEncode` and `urlDecode` are declared pure but
not bijective: `urlEncode` has a canonical output form, but
`urlDecode` accepts both `+` and `%20` as spaces, so round-tripping
is not guaranteed to preserve the input exactly.

## Compile-time proof

- No contract extractions. URL parsing is not egress; the fetch /
  service modules are where outbound hosts land in the contract.
- Labels the returned value as `user_input` because the input is
  typically the request URL. Flow analysis treats downstream uses
  as unvalidated unless passed through `decodeQuery` or similar.

## Runtime failures

- `urlParse()` throws on structurally invalid URLs. Use a
  `try-catch`-free pattern: validate the URL string first, or
  default to a known-good URL.
- `urlDecode()` throws on truncated percent sequences.

## Related

- [`zigttp:decode`](../security/decode.md) - `decodeQuery` validates
  query strings against a schema and sanitizes the `user_input`
  label.
- [`zigttp:http`](./http.md) - complementary helpers for cookies,
  content-type, and negotiation.
