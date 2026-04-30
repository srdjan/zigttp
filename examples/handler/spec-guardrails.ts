// Spec<...> example: author-declared proof obligations.
//
// `Guardrails` is a normal TS type alias whose body is the built-in
// `Spec<...>` marker imported from "zigttp:types". When the handler's
// return type intersects this alias (`Response & Guardrails`), the
// verifier runs after the analyzer pipeline and emits ZTS500 if any
// declared spec is not discharged by the inferred HandlerProperties.
//
// This handler is fully discharged: it makes no I/O, has no
// non-determinism, and never touches an env-labelled secret, so every
// member of the active spec set holds.

import type { Spec } from "zigttp:types";

type Guardrails = Spec<
    | "idempotent"
    | "deterministic"
    | "no_secret_leakage"
    | "injection_safe"
>;

function handler(req: Request): Response & Guardrails {
    return Response.json({ ok: true });
}
