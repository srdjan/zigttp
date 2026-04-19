# `zigttp:sql`

Registered SQLite queries with build-time schema validation. Queries
are named and compiled once at module scope; handlers reference them
by name.

## Summary

```ts
import { sql, sqlOne, sqlMany, sqlExec } from "zigttp:sql";

sql("listTodos", "SELECT id, title, done FROM todos ORDER BY id ASC");
sql("getTodo", "SELECT id, title, done FROM todos WHERE id = :id");
sql("createTodo", "INSERT INTO todos (title, done) VALUES (:title, 0)");

function handler(req) {
  if (req.method === "GET" && req.path === "/") {
    return Response.json({ items: sqlMany("listTodos") });
  }
  if (req.method === "POST") {
    const body = JSON.parse(req.body);
    return Response.json(sqlExec("createTodo", { title: body.title }), { status: 201 });
  }
  return Response.text("Not Found", { status: 404 });
}
```

Start the server with `--sqlite <FILE>` so the registered queries
execute against a real database:

```bash
zigttp serve examples/sql/sql-crud.ts --sqlite ./todos.db
```

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `sql` | `sql(name, statement): boolean` | register | Compile and register a named query. Module scope only. |
| `sqlOne` | `sqlOne(name, params?): row \| undefined` | read | Run a SELECT returning 0 or 1 row. |
| `sqlMany` | `sqlMany(name, params?): row[]` | read | Run a SELECT returning any number of rows. |
| `sqlExec` | `sqlExec(name, params?): { changes, lastInsertRowid }` | write | Run an INSERT, UPDATE, or DELETE. |

Parameters are bound by name using `:name` placeholders. Positional
binding is not supported.

## Compile-time proof

- Every literal `sql(name, statement)` registers the query in the
  `sql.queries` contract section with its operation kind
  (`select`, `insert`, `update`, `delete`) and the tables it
  touches.
- Pass `-Dsql-schema=<path.sql>` to validate every registered
  statement against a schema snapshot. Unknown tables or columns
  fail the build.

```bash
zig build -Dhandler=examples/sql/sql-crud.ts -Dsql-schema=examples/sql/schema.sql
```

## Runtime failures

- Unknown query names throw synchronously.
- Missing parameters throw with the placeholder name in the
  message.
- `sqlExec()` on a SELECT or `sqlOne()`/`sqlMany()` on a write
  throws with a kind-mismatch message.

## Requirements

- `--sqlite <FILE>` to point at the database file. The file is
  created on first use.
- `--sqlite :memory:` for ephemeral databases in tests.

## Related

- [`zigttp:cache`](./cache.md) - in-process cache for values that
  do not need durable storage.
- [`zigttp:validate`](../security/validate.md) - validate request
  bodies before binding them into SQL parameters.
