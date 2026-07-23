// Should fail: result.value accessed without checking result.ok
import { validateJson, schemaCompile } from "zttp:validate";
import type { Spec } from "zttp:types";

const ok = schemaCompile("test", JSON.stringify({ type: "object" }));

type Guardrails = Spec<"result_safe">;

function handler(req: Request): Response & Guardrails {
    const result = validateJson("test", req.body);
    return Response.json({ data: result.value });
}
