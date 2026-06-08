// Discriminated unions plus match narrowing (tips 5 and 6).
//
// A tagged union makes impossible states unrepresentable: every value
// carries a `kind` discriminant, so `match` narrows each arm to exactly
// one variant. There is one absent-value sentinel, `undefined`, never
// `null`. `match` exhaustiveness is checked natively, so a missing variant
// is a compile error on its own; the `default` arm below is the explicit
// catch-all. When checked, the native exhaustiveness held and the
// `assertNever(x: never)` helper was not needed (a `default` arm does not
// narrow the discriminant to `never`, so passing it to a `never` parameter
// is rejected by argument checking).

import type { Spec } from "zigttp:types";

type Command = { kind: "echo", text: string } | { kind: "ping", text: string };

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

function run(cmd: Command): string {
    return match (cmd) {
        when { kind: "echo" }:
            cmd.text
        when { kind: "ping" }:
            "pong"
        default:
            "unknown"
    };
}

function handler(req: Request): Response & Guardrails {
    const cmd: Command = { kind: "echo", text: "hi" };
    return Response.json({ result: run(cmd) });
}
