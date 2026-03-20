// Should fail: property access on optional value without checking
import { routerMatch } from "zigttp:router";

function getHome(req) {
    return Response.json({ status: "ok" });
}

const routes = {
    "GET /": getHome,
};

function handler(req: Request): Response {
    const route = routerMatch(routes, req);
    return route.handler(req);
}
