// Should pass: if (val !== undefined) narrows optional
import { env } from "zigttp:env";
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"optional_safe">;

function handler(req: Request): Response & Guardrails {
    const secret = env("SECRET");
    if (secret !== undefined) {
        return Response.json({ hasSecret: true });
    }
    return Response.json({ error: "no secret" }, { status: 500 });
}
