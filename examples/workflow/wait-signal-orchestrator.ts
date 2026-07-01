// Durable wait-signal example: the first request parks the workflow, /signal
// writes the approval payload, and the next request resumes from the signal.
//
//   zigttp serve examples/workflow/wait-signal-orchestrator.ts --durable ./.durable
import { run, signal, waitSignal } from "zigttp:durable";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "approval-demo";
  if (req.path === "/signal") {
    return Response.json({ delivered: signal(key, "approved", { approved: true }) });
  }
  return run(key, () => {
    const approval = waitSignal("approved");
    return Response.json({ resumed: true, approval });
  });
}
