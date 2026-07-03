// Co-located sub-handler for the entry-orchestrator example. Dispatched
// in-process by call()/saga() (no HTTP hop, isolated pooled runtime) - it is
// never bound to the HTTP port itself, so entry-orchestrator.ts is the only
// externally-reachable door into this bundle.
import { routerMatch } from "zigttp:router";
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"deterministic" | "no_secret_leakage" | "no_credential_leakage" | "injection_safe" | "input_validated" | "pii_contained">;

function checkStock(req: Request): Response {
  return Response.json({ inStock: true });
}

function reserve(req: Request): Response {
  return Response.json({ reserved: true });
}

function ship(req: Request): Response {
  return Response.json({ shipped: true });
}

const routes = {
  "GET /check": checkStock,
  "GET /reserve": reserve,
  "GET /ship": ship,
};

function handler(req: Request): Response & Guardrails {
  const found = routerMatch(routes, req);
  if (found !== undefined) return found.handler(req);
  return Response.json({ error: "not found" }, { status: 404 });
}
