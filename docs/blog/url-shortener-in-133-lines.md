---
title: A URL Shortener in 133 Lines of TypeScript (With Compile-Time Proofs)
date: 2026-04-05
tags: [zigttp, zigts, serverless, zig, TypeScript]
excerpt: Building a complete URL shortener with auth, caching, SQL, and input validation - where the compiler proves your handler can't leak data or miss an error check.
---

What if your compiler could look at a URL shortener and prove - mathematically, at build time - that no unvalidated user input ever reaches your database? That every error branch is handled? That no request can leak state into another?

I built one in 133 lines of TypeScript. The compiler proved all of that before a single request hit the server.

A real CRUD app: JWT auth, SQL persistence, caching, input validation, parameterized routing. It runs on zigttp, a serverless runtime where handlers are pure functions and virtual modules are the only way to touch the outside world.

## The Handler, Explained

The imports and module-scope setup first, then I'll walk through the interesting bits.

```typescript
import { guard } from "zigttp:compose";
import { routerMatch } from "zigttp:router";
import { parseBearer, jwtVerify } from "zigttp:auth";
import { env } from "zigttp:env";
import { sha256 } from "zigttp:crypto";
import { schemaCompile } from "zigttp:validate";
import { decodeJson } from "zigttp:decode";
import { sql, sqlOne, sqlExec } from "zigttp:sql";
import { cacheGet, cacheSet, cacheDelete } from "zigttp:cache";

schemaCompile("createLink", JSON.stringify({
    type: "object",
    required: ["url"],
    properties: {
        url: { type: "string", minLength: 8 },
        code: { type: "string", minLength: 3, maxLength: 16 }
    }
}));

sql("get_link", "SELECT code, url, hits, created_at FROM links WHERE code = :code");
sql("create_link", "INSERT INTO links (code, url, created_at) VALUES (:code, :url, :created_at)");
sql("increment_hits", "UPDATE links SET hits = hits + 1 WHERE code = :code");
sql("delete_link", "DELETE FROM links WHERE code = :code");
```

Eight imports from eight virtual modules. That's every dependency this handler has - no npm, no node_modules, no bundler. Each import is a Zig-native function with zero JS interpretation overhead.

The `schemaCompile` and `sql` calls sit at module scope. They run once at load time, not per request. Schema gets compiled into an internal validator, SQL statements get registered by name with parameter binding built in. By the time the first request arrives, everything is ready.

## Guard Composition: The Pipe Operator

Authentication wiring surprised me with how clean it turned out:

```typescript
const requireAuth = (req: Request): Response | undefined => {
    const token = parseBearer(req.headers["authorization"]);
    if (!token) return Response.json({ error: "missing token" }, { status: 401 });
    const secret = env("JWT_SECRET") ?? "dev-secret";
    const result = jwtVerify(token, secret);
    if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
};

// Later, in the handler:
const authed = guard(requireAuth) |> authedRouter;
return authed(req);
```

A guard is a function that returns either a Response (to short-circuit) or `undefined` (to continue). The pipe operator chains them. This is middleware, but without the framework - it's just function composition. The compiler understands guard chains and verifies that every branch returns a Response, including when guards pass through.

Public routes (health check, redirect) skip the guard entirely. Authenticated routes (create, stats, delete) go through the pipeline. Clean separation, no decorators, no middleware stack.

## The Cache-First Redirect

URL shorteners are read-heavy. Most requests are redirects, and the same short codes get hit thousands of times. So the redirect handler checks cache first:

```typescript
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
```

`cacheGet` returns `string | undefined`. The compiler requires you to narrow it before use - you can't accidentally pass `undefined` to `Response.redirect`. That truthiness check on line 4 is doing double duty: it's both the cache-hit optimization and the type narrowing the verifier demands.

Same pattern with `sqlOne`. It returns `object | undefined`. The `if (!row)` early return proves to the compiler that `row` is always defined when you access `row.url` below.

## Typed Request Ingress

The `createLink` handler is where the `injection_safe` proof comes from:

```typescript
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
```

`decodeJson` takes a schema name and the raw body. It parses JSON, validates against the compiled schema, and returns a Result. The return value carries a `validated` data flow label. When that labeled value reaches `sqlExec`, the flow checker sees: validated data going into a parameterized query. The proof holds.

If you tried passing `req.body` directly to a SQL query, the compiler would flag it. Raw user input carries the `user_input` label. Parameterized queries are fine (no string interpolation possible), but the flow analysis tracks it regardless.

The short code generation is neat too: if the user doesn't provide a custom code, `sha256` the URL and take the first 8 hex characters. Deterministic and collision-resistant.

## What the Compiler Proves

Build with `-Dverify` and the output tells you exactly what it proved:

```
Sandbox: complete (all access statically proven)
  env: restricted to [JWT_SECRET, BASE_URL] (2 proven)
  cache: restricted to [links] (1 proven)
  sql: restricted to [get_link, create_link, increment_hits, delete_link] (4 proven)

Handler Properties:
  PROVEN injection_safe   no unvalidated input in sinks
  PROVEN state_isolated   no cross-request data leakage
  PROVEN result_safe      all result.ok accesses guarded
  PROVEN optional_safe    all optionals narrowed before use
```

This means: the handler binary is sandboxed to exactly those env vars, that one cache namespace, and those four SQL queries. At runtime, any attempt to access `env("DATABASE_URL")` would fail - it's not in the proven set.

To me is interesting that this all comes from static analysis of the handler code. No annotations, no config files, no security middleware. The compiler reads the handler, tracks data flow through every branch, and either proves the properties or tells you exactly which line broke them.

## The Test Suite

The `.test.jsonl` file covers seven scenarios: health check, cache-hit redirect, cache-miss 404, missing auth token, invalid body, successful creation, and stats retrieval. Each test stubs virtual module calls with `io` entries:

```jsonl
{"type":"test","name":"POST /links creates a short link"}
{"type":"request","method":"POST","url":"/links","headers":{"authorization":"Bearer tok"},"body":"{\"url\":\"https://example.com/long-path\"}"}
{"type":"io","seq":0,"module":"auth","fn":"parseBearer","args":["Bearer tok"],"result":"tok"}
{"type":"io","seq":1,"module":"env","fn":"env","args":["JWT_SECRET"],"result":"test-secret"}
{"type":"io","seq":2,"module":"auth","fn":"jwtVerify","args":["tok","test-secret"],"result":{"ok":true,"value":{"sub":"user-1"}}}
{"type":"io","seq":3,"module":"decode","fn":"decodeJson","args":["createLink","{\"url\":\"https://example.com/long-path\"}"],"result":{"ok":true,"value":{"url":"https://example.com/long-path"}}}
{"type":"io","seq":4,"module":"crypto","fn":"sha256","args":["https://example.com/long-path"],"result":"a1b2c3d4e5f6a7b8"}
{"type":"io","seq":5,"module":"sql","fn":"sqlOne","args":["get_link",{"code":"a1b2c3d4"}],"result":null}
{"type":"io","seq":6,"module":"sql","fn":"sqlExec","args":["create_link",{"code":"a1b2c3d4","url":"https://example.com/long-path","created_at":"1700000000000"}],"result":{"rowsAffected":1}}
{"type":"io","seq":7,"module":"env","fn":"env","args":["BASE_URL"],"result":"https://sho.rt"}
{"type":"expect","status":201,"bodyContains":"a1b2c3d4"}
```

Every virtual module call is deterministic and stubbed. This is what makes replay work: record the I/O trace in production, replay it against a new handler version, and the build fails if the output changes. Handlers are pure functions of (Request, VirtualModuleResponses) - there's nowhere for non-determinism to hide.

## Tradeoffs

This approach works well for the kind of handlers FaaS is meant for: request in, response out, with some I/O in between. I explored the verification system for months before I got comfortable that the proofs catch real bugs and not just noise.

Where it falls short: the zigts subset is restrictive. No classes, no async/await, no try/catch, no while loops. If you're coming from Express or Fastify, this feels like coding with one hand tied behind your back. That's the point - the restrictions are what make verification possible - but it still takes getting used to.

The SQL module requires registering queries by name at module scope. No dynamic query building. For a URL shortener this is fine, maybe even preferable. For an app that builds complex queries based on filter combinations, you'd need a different approach.

And the ecosystem is exactly one runtime deep. No npm packages, no middleware libraries, no ORM. Everything comes from the eleven virtual modules or you write it yourself. For a focused serverless function this is a feature. For a monolith it's a non-starter.

## The Module Scorecard

Eight of eleven virtual modules in one handler, each doing something that would normally require a separate npm package:

| Module | Replaces |
|--------|----------|
| `zigttp:router` | express, fastify routing |
| `zigttp:auth` | jsonwebtoken, passport |
| `zigttp:compose` | express middleware stack |
| `zigttp:env` | dotenv |
| `zigttp:validate` + `zigttp:decode` | zod, joi, ajv |
| `zigttp:sql` | better-sqlite3, knex |
| `zigttp:cache` | node-cache, ioredis |
| `zigttp:crypto` | crypto module |

No dependencies, no cold start penalty. The binary is pre-compiled Zig with the handler bytecode embedded.

---

The handler says what it does, and the compiler proves it does only that. 133 lines, eight modules, four proofs. Whether that tradeoff is worth the restricted subset depends of what you're building. For serverless functions that handle real traffic, I'll take it.
