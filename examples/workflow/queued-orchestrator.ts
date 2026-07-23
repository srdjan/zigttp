// Durable workflow queue example: top-level call() is persisted through
// --workflow-queue before the child handler runs. The queue is for child
// dispatch; the parent response is still owned by durable.run().
//
//   zttp serve examples/workflow/queued-orchestrator.ts \
//     --system examples/workflow/system.json --durable ./.durable --workflow-queue
import { run } from "zttp:durable";
import { call } from "zttp:workflow";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "queued-demo";
  return run(key, () => {
    const res = call("greet", { method: "GET", path: "/queued" });
    return Response.json({ queued: true, subStatus: res.status, sub: res.json() });
  });
}
