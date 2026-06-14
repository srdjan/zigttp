// Should warn: unreachable code after return
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"result_safe">;

function handler(req: Request): Response & Guardrails {
    return Response.json({ ok: true });
    const x = 42;
}
