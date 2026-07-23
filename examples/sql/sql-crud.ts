import type { Spec } from "zttp:types";
import { schemaCompile, validateJson } from "zttp:validate";
import { sql, sqlExec, sqlMany } from "zttp:sql";

// A stateful (zttp:sql) handler cannot hold the default profile's
// pure/stateless/idempotent/retry_safe, so it declares the narrow set it does
// hold. Without a Spec the compiler must prove the full default profile (ZTS500).
type CrudGuarantees = Spec<
    | "deterministic"
    | "state_isolated"
    | "fault_covered"
    | "result_safe"
    | "optional_safe"
    | "no_secret_leakage"
    | "no_credential_leakage"
    | "input_validated"
    | "pii_contained"
    | "injection_safe"
    | "canonical"
>;

schemaCompile("todo.create", JSON.stringify({
    type: "object",
    required: ["title"],
    properties: {
        title: { type: "string", minLength: 1, maxLength: 120 },
    },
}));

sql("listTodos", "SELECT id, title, done FROM todos ORDER BY id ASC");
sql("createTodo", "INSERT INTO todos (title, done) VALUES (:title, 0)");

function handler(req: Request): Response & CrudGuarantees {
    if (req.method === "GET") {
        return Response.json({ items: sqlMany("listTodos") });
    }

    const parsed = validateJson("todo.create", req.body);
    if (!parsed.ok) {
        return Response.json({ errors: parsed.errors }, { status: 400 });
    }

    return Response.json(sqlExec("createTodo", { title: parsed.value.title }), { status: 201 });
}
