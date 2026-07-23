// Should pass: env() ?? "default" resolves optionality
import { env } from "zttp:env";
import type { Spec } from "zttp:types";

type Guardrails = Spec<"optional_safe">;

function handler(req: Request): Response & Guardrails {
    const name = env("APP_NAME") ?? "zttp";
    return Response.json({ app: name });
}
