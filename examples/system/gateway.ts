// Gateway handler - authenticates and routes to internal services
import { parseBearer, jwtVerify } from "zigttp:auth";
import { env } from "zigttp:env";
import { serviceCall } from "zigttp:service";

function handler(req) {
  // Authenticate
  const token = parseBearer(req.headers["authorization"]);
  if (!token) {
    return Response.json({ error: "missing token" }, { status: 401 });
  }

  const secret = env("JWT_SECRET") ?? "default-secret";
  const auth = jwtVerify(token, secret);
  if (!auth.ok) {
    return Response.json({ error: auth.error }, { status: 401 });
  }

  // Route to users service
  const user = serviceCall("users", "GET /api/users/:id", {
    params: { id: auth.value.sub },
  });
  if (!user.ok) {
    return Response.json({ error: "user service unavailable" }, { status: 502 });
  }

  return Response.json({ user: user.json() });
}
