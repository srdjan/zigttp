import { parallel } from "zttp:io";
import { fetch } from "zttp:fetch";

function fetchUser(): unknown {
  return fetch("https://users.internal/api/v1/123", {});
}

function fetchOrders(): unknown {
  return fetch("https://orders.internal/api/v1?user=123", {});
}

function fetchInventory(): unknown {
  return fetch("https://inventory.internal/api/v1/789", {});
}

function handler(_req: Request): Response {
  const results = parallel([
    fetchUser,
    fetchOrders,
    fetchInventory
  ]);
  const user = results[0];
  const orders = results[1];
  const inventory = results[2];

  return Response.json({
    user: user.ok ? user.json() : undefined,
    orders: orders.ok ? orders.json() : { items: [] },
    inventory: inventory.ok ? inventory.json() : []
  });
}
