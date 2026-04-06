# Virtual Modules Reference

All imports use the `zigttp:` prefix. Native Zig implementations with zero JS interpretation overhead.

## zigttp:env (effect: read)

```typescript
import { env } from "zigttp:env";

env(name: string): string | undefined

const dbUrl = env("DATABASE_URL") ?? "postgres://localhost";
```

## zigttp:crypto (effect: read)

```typescript
import { sha256, hmacSha256, base64Encode, base64Decode } from "zigttp:crypto";

sha256(data: string): string                    // hex-encoded
hmacSha256(key: string, data: string): string   // hex-encoded
base64Encode(data: string): string
base64Decode(data: string): string
```

## zigttp:auth (effect: read)

```typescript
import { parseBearer, jwtVerify, jwtSign, verifyWebhookSignature, timingSafeEqual } from "zigttp:auth";

parseBearer(header: string): string | undefined
jwtVerify(token: string, secret: string, options?: { clockTolerance?: number }):
    { ok: true, value: object } | { ok: false, error: string }
jwtSign(claims: string, secret: string): string
verifyWebhookSignature(payload: string, secret: string, signature: string): boolean
timingSafeEqual(a: string, b: string): boolean
```

Always check `.ok` before accessing `.value` - the verifier enforces this. `jwtSign` takes JSON-stringified claims and always returns a string (not optional). `jwtVerify` accepts an optional third argument for clock tolerance in seconds.

## zigttp:validate (effect: read)

```typescript
import { schemaCompile, validateJson, validateObject, coerceJson, schemaDrop } from "zigttp:validate";

schemaCompile(name: string, schema_json: string): boolean
validateJson(name: string, json: string): { ok: true, value: object } | { ok: false, errors: string[] }
validateObject(name: string, obj: object): { ok: true, value: object } | { ok: false, errors: string[] }
coerceJson(name: string, json: string): { ok: true, value: object } | { ok: false, errors: string[] }
schemaDrop(name: string): boolean
```

Schema supports: `type`, `required`, `properties`, `minLength`/`maxLength`, `minimum`/`maximum`, `enum`, `items`, `format`. Supported format values: `email`, `uuid`, `iso-date`, `iso-datetime`. `coerceJson` converts string numbers before validation.

## zigttp:decode (effect: read)

Typed request ingress helpers. All require a prior `schemaCompile` call for the named schema. Return values carry the `validated` label for data flow analysis.

```typescript
import { decodeJson, decodeForm, decodeQuery } from "zigttp:decode";

decodeJson(name: string, body: string): { ok: true, value: object } | { ok: false, errors: string[] }
decodeForm(name: string, body: string): { ok: true, value: object } | { ok: false, errors: string[] }
decodeQuery(name: string, query: object): { ok: true, value: object } | { ok: false, errors: string[] }
```

`decodeJson` parses JSON then validates. `decodeForm` parses URL-encoded form data (`key=val&key2=val2`, handles `+` and `%XX` encoding) then validates. `decodeQuery` validates a query object directly (typically `req.query`).

```typescript
schemaCompile("createUser", JSON.stringify({
    type: "object",
    required: ["name", "email"],
    properties: {
        name: { type: "string", minLength: 1 },
        email: { type: "string", format: "email" }
    }
}));

function handler(req: Request): Response {
    const result = decodeJson("createUser", req.body);
    if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
    return Response.json({ created: result.value }, { status: 201 });
}
```

## zigttp:cache (effect: write)

```typescript
import { cacheGet, cacheSet, cacheDelete, cacheIncr, cacheStats } from "zigttp:cache";

cacheGet(namespace: string, key: string): string | undefined
cacheSet(namespace: string, key: string, value: string, ttl?: number): boolean
cacheDelete(namespace: string, key: string): boolean
cacheIncr(namespace: string, key: string, delta?: number, ttl?: number): number
cacheStats(namespace?: string): { hits: number, misses: number, entries: number, bytes: number }
```

All values are strings (`JSON.stringify` objects). TTL in seconds. Cache persists across requests in the same pool slot. Namespace isolation with global LRU eviction.

```typescript
const cached = cacheGet("sessions", token);
if (cached) return Response.json(JSON.parse(cached));
// Compute and cache
const data = JSON.stringify(computeResult());
cacheSet("sessions", token, data, 300);
return Response.json(JSON.parse(data));
```

## zigttp:router (effect: none)

```typescript
import { routerMatch } from "zigttp:router";

routerMatch(routes: object, req: Request): { handler: function, params: object } | undefined
```

Route keys: `"METHOD /path/:param"`. Returns `undefined` on no match.

```typescript
const routes = {
    "GET /":           home,
    "GET /health":     health,
    "GET /users/:id":  getUser,
    "POST /users":     createUser,
};

function handler(req: Request): Response {
    const found = routerMatch(routes, req);
    if (found) {
        req.params = found.params;
        return found.handler(req);
    }
    return Response.json({ error: "Not Found" }, { status: 404 });
}
```

## zigttp:io (effect: write)

```typescript
import { parallel, race } from "zigttp:io";

parallel(thunks: Array<() => Response>): Array<Response>
race(thunks: Array<() => Response>): Response
```

Concurrent `fetchSync` execution. Max 8 parallel thunks. Results in declaration order for `parallel`. The runtime intercepts `fetchSync` calls during thunk execution to dispatch them concurrently.

```typescript
const [user, orders] = parallel([
    () => fetchSync(`https://users.internal/${id}`),
    () => fetchSync(`https://orders.internal?user=${id}`)
]);

return Response.json({
    user: user.ok ? user.json() : undefined,
    orders: orders.ok ? orders.json() : []
});
```

## zigttp:compose (effect: none)

```typescript
import { guard } from "zigttp:compose";

guard(fn: (req: Request) => Response | undefined): fn
```

Compile-time handler composition via pipe operator. Pre-guards receive `req`, return `Response` to short-circuit or `undefined` to continue. Post-guards (after the main handler) receive the response.

```typescript
const cors = (req: Request): Response | undefined => {
    if (req.method === "OPTIONS") {
        return Response.text("", { status: 204, headers: {
            "Access-Control-Allow-Origin": "*",
            "Access-Control-Allow-Methods": "GET, POST"
        }});
    }
};

const requireAuth = (req: Request): Response | undefined => {
    const token = parseBearer(req.headers["authorization"]);
    if (!token) return Response.json({ error: "unauthorized" }, { status: 401 });
    const result = jwtVerify(token, env("JWT_SECRET") ?? "secret");
    if (!result.ok) return Response.json({ error: result.error }, { status: 403 });
};

const handler = guard(cors)
    |> guard(requireAuth)
    |> routeHandler;
```

## zigttp:durable (effect: write)

```typescript
import { run, step, stepWithTimeout, sleep, sleepUntil,
         waitSignal, signal, signalAt } from "zigttp:durable";

run(key: string, fn: () => Response): Response
step(name: string, fn: () => unknown): unknown
stepWithTimeout(name: string, timeoutMs: number, fn: () => unknown):
    { ok: true, value: unknown } | { ok: false, error: "timeout" }
sleep(ms: number): undefined
sleepUntil(unixMs: number): undefined
waitSignal(name: string): unknown
signal(key: string, name: string, payload?: unknown): boolean
signalAt(key: string, name: string, unixMs: number, payload?: unknown): boolean
```

Crash recovery for long-running workflows. Requires `--durable <dir>`. Pending waits return 202 Accepted. Completed steps replay from recorded results on recovery.

`stepWithTimeout` wraps a step with a deadline - returns a Result so you can handle timeout without crashing the workflow.

```typescript
function handler(req: Request): Response {
    return run(`order-${req.body}`, () => {
        const validated = step("validate", () => validateOrder(req.body));
        sleep(5000);
        const charge = stepWithTimeout("charge", 10000, () =>
            fetchSync("https://payments.internal/charge", { method: "POST", body: validated })
        );
        if (!charge.ok) return Response.json({ error: "payment timeout" }, { status: 504 });
        return Response.json({ order: charge.value.json() }, { status: 201 });
    });
}
```

## zigttp:sql (effect: io)

Registered SQL queries backed by SQLite. Queries are registered by name at module scope, then executed by name in the handler. All queries use parameter binding - never string interpolation.

```typescript
import { sql, sqlOne, sqlMany, sqlExec } from "zigttp:sql";

sql(name: string, statement: string): boolean                              // register a named query
sqlOne(name: string, params?: object): object | undefined                  // single row or undefined
sqlMany(name: string, params?: object): object[]                           // array of rows
sqlExec(name: string, params?: object): { rowsAffected: number, lastRowId?: number }  // write op
```

Register queries at module scope (runs once), execute in handler (runs per request):

```typescript
sql("get_user", "SELECT * FROM users WHERE id = :id");
sql("list_users", "SELECT id, name FROM users ORDER BY name LIMIT :limit");
sql("create_user", "INSERT INTO users (name, email) VALUES (:name, :email)");

function handler(req: Request): Response {
    const user = sqlOne("get_user", { id: req.params.id });
    if (!user) return Response.json({ error: "not found" }, { status: 404 });
    return Response.json(user);
}
```
