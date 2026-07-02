// Durable wait-signal example: the first request parks the workflow, /signal
// writes the approval payload, and the next request resumes from the signal.
// /schedule instead uses signalAt to schedule delivery for a specific
// unix-ms timestamp rather than delivering immediately - useful for a
// reminder or deadline rather than an operator-driven approval. A workflow
// must already be parked in waitSignal before either signal() or signalAt()
// can deliver to it; the next /wait request after the due timestamp passes
// observes the resumed result, the same way it does after /signal.
//
//   zigttp serve examples/workflow/wait-signal-orchestrator.ts --durable ./.durable
import { run, signal, signalAt, waitSignal } from "zigttp:durable";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "approval-demo";
  if (req.path === "/signal") {
    return Response.json({ delivered: signal(key, "approved", { approved: true }) });
  }
  if (req.path === "/schedule") {
    const atMs = Date.now() - 1000; // already due, so recovery resumes it promptly
    return Response.json({ scheduled: signalAt(key, "approved", atMs, { approved: true, scheduled: true }) });
  }
  return run(key, () => {
    const approval = waitSignal("approved");
    return Response.json({ resumed: true, approval });
  });
}
