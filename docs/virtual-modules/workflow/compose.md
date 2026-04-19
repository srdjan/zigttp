# `zigttp:compose`

Compile-time handler composition with the pipe operator. `guard()`
wraps a pre- or post-check; `|>` chains them into a single flat
function.

## Summary

```ts
import { guard } from "zigttp:compose";

const withAuth = guard((req) => {
  if (req.headers["authorization"] === undefined) {
    return Response.json({ error: "unauthorized" }, { status: 401 });
  }
  return undefined;
});

const withCors = guard((res) => ({
  ...res,
  headers: { ...res.headers, "access-control-allow-origin": "*" },
}));

const handler = withAuth |> mainHandler |> withCors;
```

The parser desugars the pipe chain into one flat function at compile
time. There is no runtime indirection, no closure allocation per
request.

## API

| Export | Signature | Purpose |
|---|---|---|
| `guard` | `guard(fn): Guard` | Mark a function as a pre-guard (takes request, may return `undefined`) or post-guard (takes response, returns response). |
| `pipe` | Not called directly | Name reserved for the pipe operator (`\|>`) used by the parser. |

Pre-guards return `undefined` to pass the request through unchanged,
or return a `Response` to short-circuit. Post-guards always return
a `Response`.

## Compile-time rules

- Exactly one non-guard in the pipe chain (the main handler).
- Guards before the main handler are pre-guards (request → Response
  | undefined). Guards after are post-guards (Response → Response).
- The compiler rejects guard chains with the wrong shape at parse
  time with a `ZTS1xx` diagnostic.

## Compile-time proof

- The desugared flat function is what gets verified, so `-Dverify`
  checks each guard branch individually.
- No runtime state; `compose` is marked `comptime_only` in its
  binding.

## Runtime failures

- None. All composition happens at parse time. A malformed pipe
  chain fails the build, not a request.

## Related

- [`zigttp:router`](../http/router.md) - `routerMatch` produces the
  inner handler a guard chain wraps around.
- [`zigttp:http`](../http/http.md) - `cors()` returns a header map
  that fits naturally into a post-guard.
