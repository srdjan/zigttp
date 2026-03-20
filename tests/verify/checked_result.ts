// Should pass: result.ok checked before accessing .value
import { validateJson, schemaCompile } from "zigttp:validate";

const ok = schemaCompile("test", JSON.stringify({ type: "object" }));

function handler(req: Request): Response {
    const result = validateJson("test", req.body);
    if (result.ok) {
        return Response.json({ data: result.value });
    }
    return Response.json({ error: result.error }, { status: 400 });
}
