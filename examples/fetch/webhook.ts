// Webhook dispatch with durable replay. The handler forwards each
// POST body to an upstream billing service, keyed by the caller's
// Idempotency-Key header. A crash-restart with the same key replays
// the cached response without hitting upstream again.
//
// Run:
//   zigttp serve examples/fetch/webhook.ts \
//     --outbound-host billing.example \
//     --durable /tmp/webhook -p 3000
//
// Try it:
//   curl -X POST -H "Idempotency-Key: charge-abc-123" \
//        -d '{"amount":100}' http://localhost:3000/charge
//   # SIGKILL the server; restart.
//   curl -X POST -H "Idempotency-Key: charge-abc-123" \
//        -d '{"amount":100}' http://localhost:3000/charge
//   # The second call replays from <durable>/fetch/<hash>.step —
//   # upstream sees exactly one request for key charge-abc-123.

import { fetch } from "zigttp:fetch";

export function handler(req) {
  if (req.path !== "/charge") {
    return Response.text("not found", { status: 404 });
  }
  const key = req.headers.get("idempotency-key");
  if (!key) {
    return Response.text("idempotency-key header required", { status: 400 });
  }
  const body = req.text();
  const receipt = fetch("http://billing.example/charge", {
    method: "POST",
    headers: {
      "Idempotency-Key": key,
      "Content-Type": "application/json",
    },
    body: body,
    durable: {
      key: key,
      retries: 3,
      backoff: "exponential",
      ttl_s: 3600,
    },
  });
  if (!receipt.ok) {
    return Response.text("retry later", { status: 503 });
  }
  return Response.json(receipt.json());
}
