# `zigttp:http`

HTTP utilities for cookie parsing, cookie serialization, content
negotiation, content-type parsing, and CORS headers.

## Summary

```ts
import { parseCookies, setCookie, negotiate, cors } from "zigttp:http";

function handler(req) {
  const cookies = parseCookies(req.headers["cookie"] ?? "");
  const session = cookies["session"];

  const accept = negotiate(req.headers["accept"] ?? "", "application/json, text/html");

  const body = accept === "text/html"
    ? Response.html(`<p>hello ${session ?? "guest"}</p>`)
    : Response.json({ session });

  const corsHeaders = cors("https://app.example.com", { methods: ["GET"], credentials: true });
  for (const [k, v] of Object.entries(corsHeaders)) body.headers.set(k, v);

  body.headers.set("set-cookie", setCookie("session", "abc", {
    httpOnly: true, sameSite: "Lax", maxAgeSeconds: 3600,
  }));
  return body;
}
```

## API

| Export | Signature | Purpose |
|---|---|---|
| `parseCookies` | `parseCookies(header): Record<string,string>` | Parse a `Cookie` header into a plain object. |
| `setCookie` | `setCookie(name, value, options): string` | Build a `Set-Cookie` header value. |
| `negotiate` | `negotiate(acceptHeader, offered): string \| undefined` | Pick the best match from a comma-separated list of offered media types. |
| `parseContentType` | `parseContentType(header): { type, charset, boundary? }` | Split a `Content-Type` header into the type and its parameters. |
| `cors` | `cors(origin, options): Record<string,string>` | Build the set of CORS response headers. |

`setCookie` options: `path`, `domain`, `httpOnly`, `secure`,
`sameSite` (`"Strict" | "Lax" | "None"`), `maxAgeSeconds`, `expires`.

`cors` options: `methods`, `headers`, `credentials`, `maxAge`.

## Compile-time proof

- Literal `setCookie(name, ...)` calls populate the `cookie_name`
  contract section so the contract diff can spot removed cookies
  between versions.
- Literal `cors(origin, ...)` calls record the origin under
  `cors_origin`; a rollout that narrows the origin from `"*"` to a
  specific host is surfaced as a behavioral change.

## Runtime failures

- `parseCookies()` and `parseContentType()` return empty or partial
  objects on malformed headers rather than throwing.
- `negotiate()` returns `undefined` when no offered type matches.
- `setCookie()` throws on invalid `sameSite` values.

## Related

- [`zigttp:url`](./url.md) - URL parsing and query string handling
  for request paths and redirect targets.
- [`zigttp:text`](../platform/text.md) - HTML escaping for user
  content before interpolation into HTML responses.
