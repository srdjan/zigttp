// Durable timeout example: stepWithTimeout returns a Result. A timeout of 0ms
// makes the fixture deterministic while still exercising the timer boundary.
//
//   zttp serve examples/workflow/timeout-orchestrator.ts --durable ./.durable
import { run, sleep, stepWithTimeout } from "zttp:durable";

function handler(req: Request): Response {
  const key = req.headers.get("idempotency-key") ?? "timeout-demo";
  return run(key, () => {
    const result = stepWithTimeout("slow", 0, () => {
      sleep(1000);
      return "late";
    });
    return Response.json({ ok: result.ok, error: result.error ?? "", value: result.value ?? "" });
  });
}
