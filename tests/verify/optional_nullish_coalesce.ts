// Should pass: env() ?? "default" resolves optionality
import { env } from "zigttp:env";
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"optional_safe">;

function handler(req: Request): Response & Guardrails {
    const name = env("APP_NAME") ?? "zigttp";
    return Response.json({ app: name });
}
