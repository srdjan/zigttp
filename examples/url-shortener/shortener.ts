// URL shortener - demonstrates router, auth, decode, sql, cache, crypto, compose, env
import { guard } from "zigttp:compose";
import { routerMatch } from "zigttp:router";
import { parseBearer, jwtVerify } from "zigttp:auth";
import { env } from "zigttp:env";
import { sha256 } from "zigttp:crypto";
import { schemaCompile } from "zigttp:validate";
import { decodeJson } from "zigttp:decode";
import { sql, sqlOne, sqlExec } from "zigttp:sql";
import { cacheGet, cacheSet, cacheDelete } from "zigttp:cache";

// --- Schema (module scope, runs once) ---

schemaCompile("createLink", JSON.stringify({
    type: "object",
    required: ["url"],
    properties: {
        url: { type: "string", minLength: 8 },
        code: { type: "string", minLength: 3, maxLength: 16 }
    }
}));

// --- SQL (module scope, runs once) ---

sql("get_link", "SELECT code, url, hits, created_at FROM links WHERE code = :code");
sql("create_link", "INSERT INTO links (code, url, created_at) VALUES (:code, :url, :created_at)");
sql("increment_hits", "UPDATE links SET hits = hits + 1 WHERE code = :code");
sql("delete_link", "DELETE FROM links WHERE code = :code");

// --- Guards ---

const requireAuth = (req: Request): Response | undefined => {
    if (req.method === "OPTIONS") return undefined;
    const token = parseBearer(req.headers["authorization"]);
    if (!token) return Response.json({ error: "missing token" }, { status: 401 });
    const secret = env("JWT_SECRET") ?? "dev-secret";
    const result = jwtVerify(token, secret);
    if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
};

// --- Route handlers ---

function redirect(req: Request): Response {
    const { code } = req.params;

    const cached = cacheGet("links", code);
    if (cached) {
        sqlExec("increment_hits", { code });
        return Response.redirect(cached);
    }

    const row = sqlOne("get_link", { code });
    if (!row) return Response.json({ error: "not found" }, { status: 404 });

    cacheSet("links", code, row.url, 3600);
    sqlExec("increment_hits", { code });
    return Response.redirect(row.url);
}

function createLink(req: Request): Response {
    const input = decodeJson("createLink", req.body);
    if (!input.ok) return Response.json({ errors: input.errors }, { status: 400 });

    const code = input.value.code ?? sha256(input.value.url).slice(0, 8);

    const existing = sqlOne("get_link", { code });
    if (existing) return Response.json({ error: "code already taken" }, { status: 409 });

    const now = `${Date.now()}`;
    sqlExec("create_link", { code, url: input.value.url, created_at: now });

    const baseUrl = env("BASE_URL") ?? "http://localhost:3000";
    return Response.json({
        code,
        url: input.value.url,
        short_url: `${baseUrl}/${code}`
    }, { status: 201 });
}

function getStats(req: Request): Response {
    const { code } = req.params;
    const row = sqlOne("get_link", { code });
    if (!row) return Response.json({ error: "not found" }, { status: 404 });
    return Response.json({ code: row.code, url: row.url, hits: row.hits, created_at: row.created_at });
}

function deleteLink(req: Request): Response {
    const { code } = req.params;
    const row = sqlOne("get_link", { code });
    if (!row) return Response.json({ error: "not found" }, { status: 404 });

    sqlExec("delete_link", { code });
    cacheDelete("links", code);
    return Response.json({ deleted: code });
}

function health(_req: Request): Response {
    return Response.json({ ok: true });
}

// --- Router ---

const authedRoutes = {
    "POST /links":          createLink,
    "GET /links/:code":     getStats,
    "DELETE /links/:code":  deleteLink,
};

function authedRouter(req: Request): Response {
    const found = routerMatch(authedRoutes, req);
    if (!found) return Response.json({ error: "not found" }, { status: 404 });
    req.params = found.params;
    return found.handler(req);
}

const publicRoutes = {
    "GET /health":  health,
    "GET /:code":   redirect,
};

function handler(req: Request): Response {
    // Public routes first
    const pub = routerMatch(publicRoutes, req);
    if (pub) {
        req.params = pub.params;
        return pub.handler(req);
    }

    // Authenticated routes through guard pipeline
    const authed = guard(requireAuth) |> authedRouter;
    return authed(req);
}
