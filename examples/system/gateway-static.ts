// Gateway handler with static fetchSync URLs (for linking demo)
import { env } from "zigttp:env";

function handler(req) {
  const appName = env("APP_NAME") ?? "demo";

  // Static URL - can be linked to users service
  const health = fetchSync("https://users.internal/api/users");
  if (!health.ok) {
    return Response.json({ error: "users service down" }, { status: 502 });
  }

  // Static URL - can be linked to orders service
  const orders = fetchSync("https://orders.internal/api/orders");
  if (!orders.ok) {
    return Response.json({ error: "orders service down" }, { status: 502 });
  }

  // External URL - not part of the system
  const external = fetchSync("https://api.stripe.com/v1/charges");

  return Response.json({
    app: appName,
    users: health.json(),
    orders: orders.json(),
  });
}
