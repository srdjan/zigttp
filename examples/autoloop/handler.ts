// Intentionally insecure handler used by the autoloop smoke test.
//
// SECRET_KEY is flagged as a secret by the env virtual module, and the
// handler lets it flow straight into the response body. Run:
//
//   zigts expert --handler examples/autoloop/handler.ts \
//                --goal no_secret_leakage --max-iters 4
//
// The autoloop invokes pi_goal_check, which synthesises an executable
// counterexample request, then pi_repair_plan to propose a repair, then
// pi_apply_repair_plan to dry-run and verify the candidate. Each
// successful apply lands a chained VerifiedPatch event.
//
// pi_apply_repair_plan v1 only supports deterministic line-insertion
// edits (guard-before-line and trailing-return). Secret redaction is
// a mutation rather than an insertion, so this demo stalls: the autoloop
// honestly surfaces its iteration ceiling rather than silently pretending
// to converge. When redact_sensitive_sink lands as an insertion-friendly
// intent, the verdict flips to .achieved.
//
// Running mutates this file. Reset it before another run:
//
//   git checkout -- examples/autoloop/handler.ts

import { env } from "zigttp:env";

function handler(req: Request): Response {
    const secret = env("SECRET_KEY");
    return Response.json({ secret: secret });
}
