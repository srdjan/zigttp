// Spec<...> example: a handler that declares `idempotent` as an
// obligation but uses Date.now(), which the verifier classifies as
// non-deterministic. The build emits ZTS500 ("spec_not_discharged")
// with the per-property suggestion attached to the failure.

import type { Spec } from "zigttp:types";

type Guardrails = Spec<"idempotent" | "deterministic">;

function handler(req: Request): Response & Guardrails {
    return Response.json({ now: Date.now() });
}
