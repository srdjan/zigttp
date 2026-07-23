// Should fail: match without default arm
import type { Spec } from "zttp:types";

type Guardrails = Spec<"result_safe">;

function handler(req: Request): Response & Guardrails {
    return match (req.method) {
        when "GET": Response.json({ ok: true })
    };
}
