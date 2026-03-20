// Should pass: if (!val) return narrows via early return
import { env } from "zigttp:env";

function handler(req: Request): Response {
    const secret = env("SECRET");
    if (!secret) {
        return Response.json({ error: "no secret" }, { status: 500 });
    }
    return Response.json({ key: secret });
}
