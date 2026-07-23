// Should fail: if-without-else doesn't guarantee a return
import type { Spec } from "zttp:types";

type Guardrails = Spec<"result_safe">;

function handler(req: Request): Response & Guardrails {
    const url = req.url;
    if (url === "/health") {
        return Response.json({ status: "ok" });
    }
    // Missing: return for non-health paths
}
