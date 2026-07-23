// Minimal embedded workflow DSL example: one run key, one queued child
// boundary, and one response that names the durable pieces you can inspect.
//
// Proof first:
//   zttp check examples/workflow/dsl-orchestrator.ts --json
//   zttp check examples/workflow/dsl-orchestrator.ts --contract
//
// Then run it with persisted child dispatch:
//   zttp serve examples/workflow/dsl-orchestrator.ts \
//     --system examples/workflow/system.json --durable ./.durable --workflow-queue
import { run } from "zttp:durable";
import { call } from "zttp:workflow";
import type { Spec } from "zttp:types";

type WorkflowDslGuarantees = Spec<
    | "deterministic"
    | "state_isolated"
    | "result_safe"
    | "optional_safe"
    | "no_secret_leakage"
    | "no_credential_leakage"
    | "input_validated"
    | "pii_contained"
    | "injection_safe"
    | "canonical"
>;

function handler(req: Request): Response & WorkflowDslGuarantees {
  const key = req.headers.get("idempotency-key") ?? "workflow-dsl-demo";
  return run(key, () => {
    const res = call("greet", { method: "GET", path: "/workflow-dsl" });
    return Response.json({
      workflowDsl: true,
      runKey: key,
      childBoundary: "workflow.call:greet",
      subStatus: res.status,
      sub: res.json(),
    });
  });
}
