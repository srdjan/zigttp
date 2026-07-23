// Fan-out orchestrator: dispatch several co-located sub-handlers and aggregate
// their Responses in DECLARATION ORDER via zttp:workflow.fanout. Inside a
// durable.run the whole fan-out is recorded as ONE durable step, so on recovery
// the aggregate replays from a single oplog entry.
//
// Named `fanout` rather than `parallel` because module exports share one flat
// global name namespace, and `parallel` already belongs to zttp:io.
//
//   zttp serve examples/workflow/fanout-orchestrator.ts \
//     --system examples/workflow/system.json
import { fanout } from "zttp:workflow";

function handler(req) {
  const rs = fanout([
    { name: "greet", path: "/a" },
    { name: "greet", path: "/b" },
    { name: "greet", path: "/c" },
  ]);
  return Response.json({ n: rs.length, first: rs[0].json(), last: rs[2].json() });
}
