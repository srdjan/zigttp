import { parallel } from "zigttp:io";

function handler(req: Request): Response {
  const [user, orders, inventory] = parallel([
    () => fetchSync("https://users.internal/api/v1/123"),
    () => fetchSync("https://orders.internal/api/v1?user=123"),
    () => fetchSync("https://inventory.internal/api/v1/789")
  ]);

  return Response.json({
    user: user.ok ? user.json() : null,
    orders: orders.ok ? orders.json() : { items: [] },
    inventory: inventory.ok ? inventory.json() : []
  });
}
