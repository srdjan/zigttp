---
name: route-table
description: Add a `zttp:router` route table to an existing handler.
---
Add routing to the handler using `zttp:router`:
1. Prefer `pi_forge_route` for an added route. Convert the ask into `{ file, method, path, status? }` and let Route Forge produce a compiler-verified candidate.
2. Submit the forge candidate's returned `proposed_content` as one `apply_edit` call; do not hand-copy the generated route diff. The host compiler veto and approval policy own the write.
3. If Route Forge reports a typed blocker, read the target file, run `zts_expert_verify_paths`, and make the smallest manual route-table edit.
4. When writing manually, import `routerMatch` from `zttp:router`, check the optional match with `if (match === undefined)`, and keep handler signatures explicit.

Manual fallback shape:
```ts
import { routerMatch } from "zttp:router";

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
