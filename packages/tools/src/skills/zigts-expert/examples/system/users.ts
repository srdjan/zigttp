// Users service handler
import { cacheGet, cacheSet } from "zigttp:cache";
import { routerMatch } from "zigttp:router";
import { serviceCall } from "zigttp:service";

function getUserById(req) {
  const id = req.params.id;
  // Check cache first
  const cached = cacheGet("users", id);
  if (cached) {
    return Response.json({ user: cached });
  }

  // Fetch from orders service
  const orders = serviceCall("orders", "GET /api/orders", {
    query: { userId: id },
  });
  if (orders.status !== 200) {
    return Response.json({ error: "orders unavailable" }, { status: 502 });
  }

  const user = {
    id: id,
    name: "User " + id,
    orders: orders.json(),
  };

  cacheSet("users", id, user);
  return Response.json({ user: user });
}

function listUsers(req) {
  return Response.json({ users: [] });
}

const routes = {
  "GET /api/users/:id": getUserById,
  "GET /api/users": listUsers,
};

function handler(req) {
  const found = routerMatch(routes, req);
  if (found !== undefined) {
    req.params = found.params;
    return found.handler(req);
  }
  return Response.json({ error: "not found" }, { status: 404 });
}
