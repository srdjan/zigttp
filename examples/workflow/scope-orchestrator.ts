// Scope lifecycle example: using() ties a resource's cleanup to the request
// scope so it always runs when the scope unwinds (success or failure);
// ensure() registers a plain cleanup with no resource. scope() creates a
// named nested scope that unwinds before the function that created it
// returns, so a group of resources can close early instead of waiting for
// the whole request to finish.
//
//   zttp serve examples/workflow/scope-orchestrator.ts
//
//   GET /  -> { ok: true, name: "primary", cached: "cache-entry" }
import { scope, using, ensure } from "zttp:scope";
import { logInfo } from "zttp:log";

function handler(req: Request): Response {
  const conn = using({ name: "primary" }, (resource) => {
    logInfo("closing connection", { name: resource.name });
  });

  ensure(() => {
    logInfo("request scope unwound", {});
  });

  const cached = scope("lookup", () => {
    return using({ name: "cache-entry" }, (resource) => {
      logInfo("closing scoped resource", { name: resource.name });
    });
  });

  return Response.json({ ok: true, name: conn.name, cached: cached.name });
}
