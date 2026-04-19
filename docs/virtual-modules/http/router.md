# `zigttp:router`

Pattern-matching HTTP router that resolves a route table against
`(method, path)` and returns the matched route plus captured params.

## Summary

```ts
import { routerMatch } from "zigttp:router";

const routes = {
  "GET /": "home",
  "GET /users/:id": "user",
  "POST /users": "createUser",
  "GET /static/*": "static",
};

function handler(req) {
  const match = routerMatch(routes, { method: req.method, path: req.path });
  if (match === undefined) return Response.text("Not Found", { status: 404 });

  if (match.route === "user") {
    return Response.json({ id: match.params.id });
  }
  if (match.route === "static") {
    return Response.json({ path: match.params.splat });
  }
  return Response.json({ route: match.route });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `routerMatch` | `routerMatch(routes, { method, path }): { route, params } \| undefined` | optional object | Resolve a route table to the matched route name plus captured params. |

Pattern syntax:

- `:name` - captures a single path segment into `params.name`.
- `*` - trailing wildcard; captures the remainder into `params.splat`.
- Literal segments match exactly.

Routes are matched in order; the first matching entry wins.

## Compile-time proof

- Every literal `routes` object passed to `routerMatch()` populates
  the handler's `routes` contract section with method, pattern, and
  path params. Deploy manifests and OpenAPI emission read from this.
- The contract also captures which `match.route` branches each
  response body comes from, so response schemas in the generated
  TypeScript SDK narrow by `params.name` presence.

## Runtime failures

- Returns `undefined` when no route matches. Never throws on a
  valid routes table.
- Malformed patterns (unclosed `:`, missing segments) throw at
  registration time (the first call site for a given literal).

## Related

- [`zigttp:compose`](../workflow/compose.md) - `guard()` + `|>`
  wrap routed handlers with auth, logging, and CORS without
  sacrificing proof.
- [`zigttp:service`](../net/service.md) - when the handler's job
  is calling another service, `serviceCall()` carries the route
  pattern in a way the linker can verify.
