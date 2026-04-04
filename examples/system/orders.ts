// Orders service handler
import { cacheGet } from "zigttp:cache";
import { routerMatch } from "zigttp:router";

function getOrderById(req) {
  const cached = cacheGet("orders", req.params.id);
  if (cached) {
    return Response.json({ order: cached });
  }
  return Response.json({ error: "order not found" }, { status: 404 });
}

function listOrders(req) {
  return Response.json({ orders: [] });
}

const routes = {
  "GET /api/orders/:id": getOrderById,
  "GET /api/orders": listOrders,
};

function handler(req) {
  const found = routerMatch(routes, req);
  if (found !== undefined) {
    req.params = found.params;
    return found.handler(req);
  }
  return Response.json({ error: "not found" }, { status: 404 });
}
