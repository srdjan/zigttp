import { schemaCompile, validateJson } from "zigttp:validate";
import { sql, sqlExec, sqlMany } from "zigttp:sql";

schemaCompile("todo.create", JSON.stringify({
    type: "object",
    required: ["title"],
    properties: {
        title: { type: "string", minLength: 1, maxLength: 120 },
    },
}));

sql("listTodos", "SELECT id, title, done FROM todos ORDER BY id ASC");
sql("createTodo", "INSERT INTO todos (title, done) VALUES (:title, 0)");

function handler(req) {
    if (req.method === "GET") {
        return Response.json({ items: sqlMany("listTodos") });
    }

    const parsed = validateJson("todo.create", req.body);
    if (!parsed.ok) {
        return Response.json({ errors: parsed.errors }, { status: 400 });
    }

    return Response.json(sqlExec("createTodo", { title: parsed.value.title }), { status: 201 });
}
