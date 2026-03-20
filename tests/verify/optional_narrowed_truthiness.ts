// Should pass: if (val) narrows optional
import { env } from "zigttp:env";

function handler(req: Request): Response {
    const secret = env("SECRET");
    if (secret) {
        return Response.json({ key: secret });
    }
    return Response.json({ error: "no secret" }, { status: 500 });
}
