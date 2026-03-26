import { parallel, race } from "zigttp:io";
import { jwtVerify } from "zigttp:auth";

function handler(req: Request): Response {
  const token = req.headers.get("authorization");
  const auth = jwtVerify(token, "secret");
  if (!auth.ok) return Response.json({ error: "unauthorized" }, { status: 401 });

  // Three API calls concurrently - ~50ms instead of ~150ms
  const [user, orders, recommendations] = parallel([
    () => fetchSync(`https://users.internal/api/v1/${auth.value.sub}`),
    () => fetchSync(`https://orders.internal/api/v1?user=${auth.value.sub}&limit=10`),
    () => fetchSync(`https://ml.internal/api/v1/recommend/${auth.value.sub}`)
  ]);

  return Response.json({
    user: user.ok ? user.json() : null,
    orders: orders.ok ? orders.json() : { items: [] },
    recommendations: recommendations.ok ? recommendations.json() : []
  });
}
