---
name: sql-query
description: Add a typed SQL query pattern using zigttp:sql.
---
Add a SQL query using `zigttp:sql`:
```js
import { sqlOne, sqlMany } from "zigttp:sql";

// Single row (returns undefined if not found)
const user = sqlOne("SELECT id, name FROM users WHERE id = ?", [id]);
if (!user) return Response.json({ error: "not_found" }, { status: 404 });

// Multiple rows
const items = sqlMany("SELECT * FROM items WHERE owner = ?", [userId]);
```
Always parameterize queries. Never interpolate user input into SQL strings.
Use `sql()` for writes (INSERT/UPDATE/DELETE) that don't return rows.
