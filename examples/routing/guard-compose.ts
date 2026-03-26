// Guard composition example
// Demonstrates left-to-right guard chain with compile-time desugaring

import { guard } from "zigttp:compose";
import { routerMatch } from "zigttp:router";
import { env } from "zigttp:env";
import { parseBearer, jwtVerify } from "zigttp:auth";

function preflight(req: Request): Response | undefined {
    if (req.method === "OPTIONS") {
        return Response.text("", {
            status: 204,
            headers: {
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "GET, POST",
                "Access-Control-Allow-Headers": "Authorization, Content-Type",
            }
        });
    }
}

function requireAuth(req: Request): Response | undefined {
    const header = req.headers["authorization"] ?? "";
    const token = parseBearer(header);
    if (!token) return Response.json({error: "unauthorized"}, {status: 401});
    const secret = env("JWT_SECRET") ?? "secret";
    const result = jwtVerify(token, secret);
    if (!result.ok) return Response.json({error: result.error}, {status: 403});
}

// Route handlers
function getHealth(req: Request): Response {
    return Response.json({healthy: true});
}

function getUser(req: Request): Response {
    return Response.json({id: req.params.id});
}

const routes = {
    "GET /health": getHealth,
    "GET /users/:id": getUser,
};

function routeHandler(req: Request): Response {
    const found = routerMatch(routes, req);
    if (found !== undefined) {
        req.params = found.params;
        return found.handler(req);
    }
    return Response.json({error: "Not Found"}, {status: 404});
}

// Composed handler: preflight -> auth -> routes
const handler = guard(preflight)
    |> guard(requireAuth)
    |> routeHandler;
