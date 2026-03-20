// Should pass: if (val !== undefined) narrows optional
import { env } from "zigttp:env";

function handler(req: Request): Response {
    const secret = env("SECRET");
    if (secret !== undefined) {
        return Response.json({ key: secret });
    }
    return Response.json({ error: "no secret" }, { status: 500 });
}
