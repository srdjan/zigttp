// Should fail: result.value accessed without checking result.ok
import { validateJson, schemaCompile } from "zigttp:validate";

const ok = schemaCompile("test", JSON.stringify({ type: "object" }));

function handler(req: Request): Response {
    const result = validateJson("test", req.body);
    return Response.json({ data: result.value });
}
