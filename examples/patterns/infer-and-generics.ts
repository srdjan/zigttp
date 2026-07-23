// Annotate the boundary, infer the internals, and reuse a generic helper.
//
// zts requires parameter and return annotations on named functions (the
// boundary), then infers locals from their initializers, so redundant local
// annotations are unnecessary. A generic helper such as
// `first<T>(xs: T[]): T | undefined` is reused across element types by naming
// the type argument at the call site. The single absent-value sentinel is
// `undefined`, never `null`.

import type { Spec } from "zttp:types";

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

// Generic helper: annotate the boundary once, reuse for any element type.
function first<T>(xs: T[]): T | undefined {
    for (const x of xs) {
        return x;
    }
    return undefined;
}

function handler(req: Request): Response & Guardrails {
    const names: string[] = ["alice", "bob", "carol"];
    // Locals infer from their initializers; the helper is reused via `first<string>`.
    const head: string | undefined = first<string>(names);
    if (head === undefined) {
        return Response.json({ count: 0 }, { status: 404 });
    }
    return Response.json({ first: head, count: names.length });
}
