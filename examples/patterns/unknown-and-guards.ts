// Prefer `unknown` over `any`, and narrow before use.
//
// zigts rejects `any` outright, so a value of unknown shape is typed
// `unknown`. The only way to use it is to narrow it first: a `typeof`
// check narrows `unknown` to a concrete type inside the then-branch, and
// the value is unusable until then. This covers tips 1 (prefer `unknown`)
// and 8 (narrowing an unknown value before use).

import type { Spec } from "zigttp:types";

type Guardrails = Spec<
    | "deterministic"
    | "read_only"
    | "retry_safe"
    | "idempotent"
    | "state_isolated"
    | "injection_safe"
    | "no_secret_leakage"
    | "input_validated"
>;

function handler(req: Request): Response & Guardrails {
    const body: unknown = req.body;

    // `body` is `unknown` here; the `typeof` check narrows it to `string`
    // inside the branch, where `.length` is valid.
    if (typeof body === "string") {
        return Response.json({ length: body.length });
    }

    return Response.json({ error: "expected string body" }, { status: 400 });
}
