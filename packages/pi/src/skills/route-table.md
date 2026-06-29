---
name: route-table
description: Add a `zigttp:router` route table to an existing handler.
---
Add routing to the handler using `zigttp:router`:
1. Prefer `pi_forge_route` for an added route. Convert the ask into `{ file, method, path, status? }` and let Route Forge produce a compiler-verified candidate.
2. Apply an approved forge candidate through `pi_apply_feature_plan`; do not hand-copy the generated route diff.
3. If Route Forge reports a typed blocker, read the target file, run `zigts_expert_verify_paths`, and make the smallest manual route-table edit.
4. When writing manually, import `routerMatch` from `zigttp:router`, check the optional match with `if (match === undefined)`, and keep handler signatures explicit.

Manual fallback shape:
```ts
import { routerMatch } from "zigttp:router";

function handleIndex(req: Request, params: object): Response {
  return Response.json({ ok: true });
}

const routes = {
  "GET /": handleIndex,
};

function handler(req: Request): Response {
  const match = routerMatch(routes, req);
  if (match === undefined) {
    return Response.json({ error: "not_found" }, { status: 404 });
  }
  return match.handler(req, match.params);
}
```

Each route handler receives `(req, params)`. Path params use `:name` syntax.
