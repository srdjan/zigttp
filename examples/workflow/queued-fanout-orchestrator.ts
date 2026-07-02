// Queued fan-out example: workflow.fanout() dispatched inside a durable.run
// with --workflow-queue persists each child call before it runs, the same
// crash-tolerance top-level call() already gets in queued-orchestrator.ts.
// Fanout only takes the queued dispatch path at step depth 0 inside a
// durable run - a top-level (non-durable) fanout(), like
// fanout-orchestrator.ts, always dispatches directly regardless of
// --workflow-queue.
//
//   zigttp serve examples/workflow/queued-fanout-orchestrator.ts \
//     --system examples/workflow/system.json --durable ./.durable --workflow-queue
import { run } from "zigttp:durable";
import { fanout } from "zigttp:workflow";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "queued-fanout-demo";
  return run(key, () => {
    const rs = fanout([
      { name: "greet", path: "/a" },
      { name: "greet", path: "/b" },
    ]);
    return Response.json({ n: rs.length, first: rs[0].json(), last: rs[1].json() });
  });
}
