// HATEOAS follow: the orchestrator builds a structured resource() carrying
// affordances, then dispatches one by `rel` via zttp:workflow.follow without
// hardcoding the target handler name. follow() resolves the affordance's href
// to a co-located sub-handler by the "/<name>" mount convention - here the href
// "/greet" routes to the greet sub-handler - and returns its Response copied
// into orchestrator-owned memory. Inside a durable.run the routed dispatch is
// recorded as one durable step, so it replays from the oplog on recovery.
//
//   zttp serve examples/workflow/follow-orchestrator.ts \
//     --system examples/workflow/system.json
import { follow } from "zttp:workflow";

function handler(req) {
  const home = resource({ service: "orchestrator" }, {
    self: { href: "/" },
    greeting: { href: "/greet", method: "GET" },
  });
  return follow(home, "greeting");
}
