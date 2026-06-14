// Should pass: if (!val) return narrows via early return
import { env } from "zigttp:env";
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"optional_safe">;

function handler(req: Request): Response & Guardrails {
    const secret = env("SECRET");
    if (!secret) {
        return Response.json({ error: "no secret" }, { status: 500 });
    }
    return Response.json({ hasSecret: true });
}
