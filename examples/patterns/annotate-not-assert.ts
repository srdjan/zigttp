// Annotate the declaration instead of asserting with `as` or `satisfies`,
// and declare the type alias first instead of deriving it with `typeof`.
//
// zigts rejects both `as` and `satisfies`. The conformance check moves onto
// the binding: `const config: Config = {...}` checks the literal against
// `Config` and keeps its narrow type, which is what `satisfies` was for. The
// reverse direction (`type Config = typeof config`) is unsupported, so the
// alias is the single source of truth and the value is checked against it.

import type { Spec } from "zigttp:types";

type Config = { port: number; host: string; readonly version: string };

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

// Annotated `const`: no `as`, no `satisfies`. The annotation checks the
// literal against `Config` and pins the narrow type on its own.
const config: Config = { port: 8080, host: "0.0.0.0", version: "1.0" };

function handler(req: Request): Response & Guardrails {
    const port: number = config.port;
    const host: string = config.host;
    const version: string = config.version;
    return Response.json({ port: port, host: host, version: version });
}
