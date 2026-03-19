// Router module example
// Demonstrates pattern-matching HTTP routing with path parameters

import { routerMatch } from "zigttp:router";
import { sha256 } from "zigttp:crypto";
import { env } from "zigttp:env";

function getHome(req) {
    return Response.json({ name: env("APP_NAME"), status: "ok" });
}

function getHealth(req) {
    return Response.json({ healthy: true });
}

function getUser(req) {
    return Response.json({ id: req.params.id, hash: sha256(req.params.id) });
}

function postEcho(req) {
    return Response.json({ received: req.body, hash: sha256(req.body) });
}

const routes = {
    "GET /": getHome,
    "GET /health": getHealth,
    "GET /users/:id": getUser,
    "POST /echo": postEcho,
};

function handler(req) {
    const match = routerMatch(routes, req);
    if (match) {
        req.params = match.params;
        return match.handler(req);
    }
    return Response.json({ error: "Not Found" }, { status: 404 });
}
