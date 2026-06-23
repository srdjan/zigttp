import { parallel } from "zigttp:io";
import { jwtVerify } from "zigttp:auth";
import { env } from "zigttp:env";
import { fetch } from "zigttp:fetch";

function handler(req: Request): Response {
  const token = req.headers.get("authorization");
  const secret = env("JWT_SECRET");
  if (secret === undefined) {
    return Response.json({ error: "server misconfigured" }, { status: 500 });
  }

  const auth = jwtVerify(token, secret);
  if (!auth.ok) return Response.json({ error: "unauthorized" }, { status: 401 });
  const subject = auth.value.sub;

  function _fetchUser(): unknown {
    return fetch("https://users.internal/api/v1/" + subject, {});
  }

  function _fetchOrders(): unknown {
    return fetch("https://orders.internal/api/v1?user=" + subject + "&limit=10", {});
  }

  function _fetchRecommendations(): unknown {
    return fetch("https://ml.internal/api/v1/recommend/" + subject, {});
  }

  // Three API calls concurrently - ~50ms instead of ~150ms
  const results = parallel([
    _fetchUser,
    _fetchOrders,
    _fetchRecommendations
  ]);
  const user = results[0];
  const orders = results[1];
  const recommendations = results[2];

  return Response.json({
    user: user.ok ? user.json() : undefined,
    orders: orders.ok ? orders.json() : { items: [] },
    recommendations: recommendations.ok ? recommendations.json() : []
  });
}
