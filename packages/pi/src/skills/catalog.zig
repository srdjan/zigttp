//! Baked-in skill catalog. Each Skill carries a name, a short description
//! shown in help/meta output, and a body that is injected as a user-role
//! message when the user invokes `/skill:<name>`. No external files are read;
//! everything lives in this source file.

pub const Skill = struct {
    name: []const u8,
    description: []const u8,
    body: []const u8,
};

pub const catalog = [_]Skill{
    .{
        .name = "handler-scaffold",
        .description = "Scaffold a minimal zigts handler with typed request parsing and error handling.",
        .body =
        \\Create a minimal zigts handler following these conventions:
        \\- Use typed destructuring for request params (query, body, path).
        \\- All errors must be returned as structured JSON responses, never thrown.
        \\- Import only the specific zigttp modules you need.
        \\- Use `zigttp:decode` for request parsing and `zigttp:validate` for schema validation.
        \\- Use `Response.json()` for all JSON responses, `Response.text()` for plain text.
        \\- No classes, no async/await, no try/catch. Use `match` for error branching.
        \\Start with the minimal passing scaffold and add complexity only as needed.
        ,
    },
    .{
        .name = "fix-violations",
        .description = "Fix all compiler violations in the current file by iterating until clean.",
        .body =
        \\Work through every compiler violation in the current handler file systematically:
        \\1. Run `/check` to get the full violation list.
        \\2. Fix the highest-priority violations first (ZTS001-ZTS099 before ZTS100+).
        \\3. After each edit, run `/verify <file>` to confirm the fix landed.
        \\4. Continue until `/check` reports zero violations.
        \\Do not introduce new violations while fixing existing ones. Use `--diff-only` mode.
        ,
    },
    .{
        .name = "route-table",
        .description = "Add a `zigttp:router` route table to an existing handler.",
        .body =
        \\Add routing to the handler using `zigttp:router`:
        \\```js
        \\import { routerMatch } from "zigttp:router";
        \\
        \\const routes = {
        \\  "GET /": handleIndex,
        \\  "POST /items": handleCreate,
        \\};
        \\
        \\function handler(req) {
        \\  const match = routerMatch(routes, req);
        \\  if (!match) return Response.json({ error: "not_found" }, { status: 404 });
        \\  return match.handler(req, match.params);
        \\}
        \\```
        \\Each route handler receives `(req, params)`. Path params use `:name` syntax.
        ,
    },
    .{
        .name = "auth-jwt",
        .description = "Add JWT verification to a handler using zigttp:auth.",
        .body =
        \\Add JWT auth to the handler:
        \\1. Import `parseBearer` and `jwtVerify` from `zigttp:auth`.
        \\2. Extract the token: `const token = parseBearer(req.headers.get("authorization"))`.
        \\3. Verify: `const claims = jwtVerify(token, env.JWT_SECRET)`.
        \\4. Return 401 on verification failure: `if (!claims.ok) return Response.json({ error: "unauthorized" }, { status: 401 })`.
        \\5. Use `claims.value.sub` (or other standard claims) in the handler body.
        \\Add `JWT_SECRET` to the env contract. Never log or expose the raw token.
        ,
    },
    .{
        .name = "sql-query",
        .description = "Add a typed SQL query pattern using zigttp:sql.",
        .body =
        \\Add a SQL query using `zigttp:sql`:
        \\```js
        \\import { sqlOne, sqlMany } from "zigttp:sql";
        \\
        \\// Single row (returns undefined if not found)
        \\const user = sqlOne("SELECT id, name FROM users WHERE id = ?", [id]);
        \\if (!user) return Response.json({ error: "not_found" }, { status: 404 });
        \\
        \\// Multiple rows
        \\const items = sqlMany("SELECT * FROM items WHERE owner = ?", [userId]);
        \\```
        \\Always parameterize queries. Never interpolate user input into SQL strings.
        \\Use `sql()` for writes (INSERT/UPDATE/DELETE) that don't return rows.
        ,
    },
};

pub fn findByName(name: []const u8) ?*const Skill {
    for (&catalog) |*skill| {
        if (std.mem.eql(u8, skill.name, name)) return skill;
    }
    return null;
}

const std = @import("std");
