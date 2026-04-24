// Deliberately insecure handler used as a fixture for counterexample tests.
// Reads a secret-labelled env var and echoes it into the response body.
// flow_checker should emit `secret_in_response` and the counterexample
// synthesiser should produce an executable witness that drives this path.

import { env } from "zigttp:env";

function handler(req: Request): Response {
  const secret = env("SECRET_KEY");
  if (secret) {
    return Response.json({ leaked: secret });
  }
  return Response.json({ ok: true });
}
