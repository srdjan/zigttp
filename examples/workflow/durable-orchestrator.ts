// Durable orchestrator: wraps the in-process sub-handler call in durable.run.
// Each workflow.call is recorded as its own durable step, so on crash recovery
// a COMPLETED sub-call replays from the oplog instead of being re-dispatched
// (don't re-charge a payment). Serve with: --system + --durable <DIR>.
//
//   zigttp serve examples/workflow/durable-orchestrator.ts \
//     --system examples/workflow/system.json --durable ./.durable
import { run } from "zigttp:durable";
import { call } from "zigttp:workflow";

function handler(req: Request): Response {
  const key = req.headers["idempotency-key"] ?? "default";
  return run(key, () => {
    const res = call("greet", { method: "GET", path: "/greet" });
    return Response.json({ orchestrated: true, subStatus: res.status, sub: res.json() });
  });
}
