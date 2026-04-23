---
name: route-table
description: Add a `zigttp:router` route table to an existing handler.
---
Add routing to the handler using `zigttp:router`:
```js
import { routerMatch } from "zigttp:router";

const routes = {
  "GET /": handleIndex,
  "POST /items": handleCreate,
};

function handler(req) {
  const match = routerMatch(routes, req);
  if (!match) return Response.json({ error: "not_found" }, { status: 404 });
  return match.handler(req, match.params);
}
```
Each route handler receives `(req, params)`. Path params use `:name` syntax.
