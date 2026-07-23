// Should fail: optional value used without checking for undefined
import { env } from "zttp:env";
import type { Spec } from "zttp:types";

type Guardrails = Spec<"optional_safe">;

function handler(req: Request): Response & Guardrails {
    const secret = env("SECRET");
    return Response.json({ key: secret });
}
