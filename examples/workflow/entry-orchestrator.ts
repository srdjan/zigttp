// Entry orchestrator: the single externally-reachable door into this bundle.
// Its own route surface is POST-only (proven via `properties.postOnly` in
// `zigttp check --contract`), and it dispatches to named co-located
// sub-handlers via zigttp:workflow - proven by `zigttp link` the same way
// serviceCall targets are proven, closing the compile-time gap a typo like
// call("inventroy", ...) used to only surface as a runtime 599.
//
//   zigttp link examples/workflow/entry-system.json
//     -> resolves every call() target against inventory.ts's routes
//     -> validates the declared "entry" names a real bundle handler
//     -> signs a kind=workflow receipt attesting to both
//
//   zigttp serve examples/workflow/entry-orchestrator.ts \
//     --system examples/workflow/entry-system.json
//
//   POST /orders  -> reserve then ship, as two in-process calls
import { routerMatch } from "zigttp:router";
import { call } from "zigttp:workflow";
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"deterministic" | "no_secret_leakage" | "no_credential_leakage" | "injection_safe" | "input_validated" | "pii_contained">;

function createOrder(req: Request): Response {
  const reserved = call("inventory", { method: "GET", path: "/reserve" });
  const shipped = call("inventory", { method: "GET", path: "/ship" });
  return Response.json({ reservedStatus: reserved.status, shippedStatus: shipped.status });
}

const routes = {
  "POST /orders": createOrder,
};

function handler(req: Request): Response & Guardrails {
  const found = routerMatch(routes, req);
  if (found !== undefined) return found.handler(req);
  return Response.json({ error: "not found" }, { status: 405 });
}
