// Saga orchestrator: a sequence of in-process sub-handler calls made atomic by
// reverse-order compensation. Each step's `run` is recorded as a durable step
// "do:<name>"; if a step fails (Response status >= 400) the already-completed
// steps are rolled back in reverse via their `compensate` thunks ("undo:<name>").
// On crash recovery the completed do:/undo: steps replay from the oplog, so the
// rollback is reproduced without re-invoking anything.
//
//   zigttp serve examples/workflow/saga-orchestrator.ts \
//     --system examples/workflow/system.json --durable ./.durable
//
//   GET /      -> every step succeeds -> { ok: true }
//   GET /fail  -> "charge" is declined (402) -> "reserve" is compensated
//
// Notes: the handler is left without explicit return-type annotations because
// the step thunks return zigttp:workflow.call results (typed `object`), which
// the strict Response-return check would reject (strict-ZigTS is advisory). The
// two branches inline literal charge paths rather than capturing a variable in
// the step thunks, sidestepping a deep-closure upvalue limitation.
import { run } from "zigttp:durable";
import { call, saga } from "zigttp:workflow";

function handler(req) {
  const key = req.headers.get("idempotency-key") ?? "saga-demo";
  if (req.url === "/fail") {
    return run(key, () =>
      saga([
        { name: "reserve", run: () => call("greet", { path: "/reserve" }), compensate: () => call("greet", { path: "/release" }) },
        { name: "charge", run: () => call("greet", { path: "/decline" }), compensate: () => call("greet", { path: "/refund" }) },
        { name: "ship", run: () => call("greet", { path: "/ship" }) },
      ]),
    );
  }
  return run(key, () =>
    saga([
      { name: "reserve", run: () => call("greet", { path: "/reserve" }), compensate: () => call("greet", { path: "/release" }) },
      { name: "charge", run: () => call("greet", { path: "/charge" }), compensate: () => call("greet", { path: "/refund" }) },
      { name: "ship", run: () => call("greet", { path: "/ship" }) },
    ]),
  );
}
