// Should fail: property access on optional value without checking
import { routerMatch } from "zigttp:router";
import type { Spec } from "zigttp:types";

function getHome(req: Request): Response {
    return Response.json({ status: "ok" });
}

const routes = {
    "GET /": getHome,
};

type Guardrails = Spec<"optional_safe">;

function handler(req: Request): Response & Guardrails {
    const route = routerMatch(routes, req);
    return route.handler(req);
}
