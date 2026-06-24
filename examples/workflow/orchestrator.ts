// Orchestrator handler: composes a co-located sub-handler in-process with
// zigttp:workflow.call. The sub-handler runs in its own isolated pooled
// runtime; its Response is copied back into this handler before continuing.
//
// Route "/panic" targets a sub-handler path the panic-injection e2e wires to
// a forced panic, proving that a sub-handler panic surfaces as a failed call
// (subStatus 599) and does NOT take down the orchestrator.
import { call } from "zigttp:workflow";

function handler(req: Request): Response {
  const subPath = req.url === "/panic" ? "/boom" : "/greet";
  const res = call("greet", { method: "GET", path: subPath });
  return Response.json({ orchestrated: true, subStatus: res.status, sub: res.json() });
}
