// Co-located sub-handler, dispatched in-process by the orchestrator via
// zigttp:workflow.call (no HTTP, isolated pooled runtime). It echoes the
// method and path the orchestrator handed it, and returns 402 for "/decline"
// so the saga example can drive a deterministic step failure.
function handler(req: Request): Response {
  if (req.url === "/decline") return Response.json({ error: "declined" }, { status: 402 });
  return Response.json({ from: "greet", method: req.method, path: req.url });
}
