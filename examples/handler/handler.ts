// TypeScript handler example with author-declared proof obligations.
//
// `Spec<...>` from "zttp:types" is a phantom marker the verifier
// reads off the return type and discharges against the inferred
// HandlerProperties. The compiler emits ZTS500 if any declared spec
// does not hold. Witnesses for failing counterexample-rich specs
// persist to .zttp/witnesses/ so future builds replay them.

import type { Spec } from "zttp:types";

type Guardrails = Spec<
    | "injection_safe"
    | "state_isolated"
    | "retry_safe"
    | "no_secret_leakage"
>;

type RequestData = {
    name: string;
    count: number;
};

interface ResponseData {
    message: string;
    timestamp: number;
}

function processData(data: RequestData): ResponseData {
    return {
        message: "Hello, " + data.name,
        timestamp: Date.now()
    };
}

function handler(req: Request): Response & Guardrails {
    const data: RequestData = { name: "World", count: 42 };
    const result: ResponseData = processData(data);
    return Response.json(result);
}
