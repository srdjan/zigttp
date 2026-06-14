// Should pass: if/else both return
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"result_safe">;

function handler(req: Request): Response & Guardrails {
    const url = req.url;
    if (url === "/health") {
        return Response.json({ status: "ok" });
    } else {
        return Response.json({ error: "not found" }, { status: 404 });
    }
}
