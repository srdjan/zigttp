// Validate external data at runtime instead of reaching for Zod.
//
// There is no Zod step. Compile a schema by name at the top level with
// `zigttp:validate`, then gate the handler on the `.ok` of the result
// before touching `.value`. The verifier enforces the `.ok`-before-`.value`
// discipline at build time: `validateJson` is a Result-producing call, so
// accessing `.value` on an unchecked result is a compile error.

import { schemaCompile, validateJson } from "zigttp:validate";
import type { Spec } from "zigttp:types";

schemaCompile("todo", '{"type":"object","required":["title"]}');

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
    const parsed = validateJson("todo", req.body);
    if (!parsed.ok) {
        return Response.json({ error: "invalid body" }, { status: 400 });
    }
    return Response.json(parsed.value, { status: 201 });
}
