// Gateway handler with static fetchSync URLs (for linking demo)
import { env } from "zigttp:env";
import { serviceCall } from "zigttp:service";

function handler(req) {
  const appName = env("APP_NAME") ?? "demo";

  // Named internal call - linked directly to users service
  const health = serviceCall("users", "GET /api/users", {});
  if (!health.ok) {
    return Response.json({ error: "users service down" }, { status: 502 });
  }

  // Named internal call - linked directly to orders service
  const orders = serviceCall("orders", "GET /api/orders", {});
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
