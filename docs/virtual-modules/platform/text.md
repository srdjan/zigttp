# `zigttp:text`

String utilities: HTML escape/unescape, slugify, truncate, mask.
Pure functions with no side effects.

## Summary

```ts
import { escapeHtml, slugify, mask } from "zigttp:text";

function handler(req) {
  const title = req.query["title"] ?? "untitled";
  const slug = slugify(title);
  const safeTitle = escapeHtml(title);
  const shownKey = mask(env("API_KEY") ?? "", 4);

  return Response.html(`
    <article data-slug="${slug}">
      <h1>${safeTitle}</h1>
      <footer>key: ${shownKey}</footer>
    </article>
  `);
}
```

## API

| Export | Signature | Purpose |
|---|---|---|
| `escapeHtml` | `escapeHtml(input): string` | Escape `& < > " '` for safe HTML interpolation. Labels output `validated`. |
| `unescapeHtml` | `unescapeHtml(input): string` | Inverse of `escapeHtml`. Not bijective: `&amp;` and `&#38;` both decode to `&`. |
| `slugify` | `slugify(input): string` | Lowercase, ASCII-fold, collapse non-alnum to `-`. |
| `truncate` | `truncate(input, maxLength, ellipsis): string` | Trim to `maxLength` chars, append ellipsis if trimmed. |
| `mask` | `mask(input, visible): string` | Keep the last `visible` chars, replace the rest with `*`. Labels output `internal`. |

All five are pure; canonicalization may dedupe repeated calls with
literal arguments.

## Compile-time proof

- `escapeHtml()` is a sanitization barrier for the `user_input`
  flow label: output lands safely in response bodies. Without it,
  the flow checker flags HTML interpolation as a potential XSS
  vector.
- `mask()` is the sanitizer for `secret` / `credential` labels
  before logging or response inclusion.

## Runtime failures

- Non-string input throws a TypeError.
- `truncate()` and `mask()` throw on non-integer numeric arguments.

## Related

- [`zigttp:http`](../http/http.md) - `parseCookies()` returns
  user-supplied strings; escape them before HTML interpolation.
- [`zigttp:log`](./log.md) - use `mask()` on secret-shaped values
  before `logInfo()`.
