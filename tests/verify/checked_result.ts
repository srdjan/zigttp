// Should pass: result.ok checked before accessing .value
import { validateJson, schemaCompile } from "zigttp:validate";
import type { Spec } from "zigttp:types";

const ok = schemaCompile("test", JSON.stringify({ type: "object" }));

type Guardrails = Spec<"result_safe">;

function handler(req: Request): Response & Guardrails {
    const result = validateJson("test", req.body);
    if (result.ok) {
        return Response.json({ data: result.value });
    }
    return Response.json({ error: result.error }, { status: 400 });
}
