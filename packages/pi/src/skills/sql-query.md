---
name: sql-query
description: Add a typed SQL query pattern using zttp:sql.
---
Add a SQL query using `zttp:sql`:
1. Check `zttp.json` first. SQL verification requires a configured `sqlite` schema/database; if it is missing, ask for configuration or use `zttp:cache` instead.
2. Register statements with `sql(name, statement)` at module scope.
3. Use named SQL parameters such as `:id` and pass an object like `{ id }`. Positional `?` parameters are rejected.
4. Use `sqlOne(name, params?)` for one optional row, `sqlMany(name, params?)` for arrays, and `sqlExec(name, params?)` for writes.
5. Stateful SQL handlers need a narrow `Spec<...>`; do not claim `read_only`, `idempotent`, or `retry_safe` for mutation paths.

```ts
import type { Spec } from "zttp:types";
import { sql, sqlOne, sqlExec } from "zttp:sql";

type SqlWriteSpec = Spec<"deterministic" | "state_isolated" | "fault_covered" | "canonical">;

sql("getUser", "SELECT id, name FROM users WHERE id = :id");
sql("touchUser", "UPDATE users SET seen = 1 WHERE id = :id");

function handler(req: Request): Response & SqlWriteSpec {
  const user = sqlOne("getUser", { id: "u_123" });
  if (user === undefined) {
    return Response.json({ error: "not_found" }, { status: 404 });
  }
  return Response.json(sqlExec("touchUser", { id: user.id }));
}
```

Never interpolate user input into SQL strings. Validate request data before passing it as named parameters.
