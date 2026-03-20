// Should pass: env() ?? "default" resolves optionality
import { env } from "zigttp:env";

function handler(req: Request): Response {
    const name = env("APP_NAME") ?? "zigttp";
    return Response.json({ app: name });
}
