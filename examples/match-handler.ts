// Match expression handler example
//
// Demonstrates using match for HTTP routing.

import { env } from "zigttp:env";

function handler(req: Request): Response {
    return match (req) {
        when { method: "GET", path: "/health" }:
            Response.json({ ok: true })
        when { method: "GET", path: "/version" }:
            Response.json({ version: "1.0.0" })
        when { method: "POST", path: "/echo" }:
            Response.json(req.body)
        default:
            Response.text("Not Found", { status: 404 })
    };
}
