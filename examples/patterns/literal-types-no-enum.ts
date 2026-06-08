// Literal union types instead of enums, plus template literal types.
//
// zigts does not support `enum` (use a literal union: a finite set of
// string literals joined with `|`). It also rejects `as const`: a `const`
// binding already preserves its literal type from the annotation, so no
// assertion is needed. Template literal types constrain a string to a
// pattern, here any path under `/api/`.

import type { Spec } from "zigttp:types";

type Method = "GET" | "POST" | "DELETE";
type ApiRoute = `/api/${string}`;

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

// No `as const` here. The annotation pins the literal type on its own.
const defaultMethod: Method = "GET";
const defaultRoute: ApiRoute = "/api/health";

function handler(req: Request): Response & Guardrails {
    const method: Method = defaultMethod;
    const route: ApiRoute = defaultRoute;
    return Response.json({ method: method, route: route });
}
