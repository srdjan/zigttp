// Should fail: optional value used without checking for undefined
import { env } from "zigttp:env";

function handler(req: Request): Response {
    const secret = env("SECRET");
    return Response.json({ key: secret });
}
