---
name: auth-jwt
description: Add JWT verification to a handler using zigttp:auth.
---
Add JWT auth to the handler:
1. Import `parseBearer` and `jwtVerify` from `zigttp:auth`.
2. Extract the token: `const token = parseBearer(req.headers.get("authorization"))`.
3. Verify: `const claims = jwtVerify(token, env.JWT_SECRET)`.
4. Return 401 on verification failure: `if (!claims.ok) return Response.json({ error: "unauthorized" }, { status: 401 })`.
5. Use `claims.value.sub` (or other standard claims) in the handler body.
Add `JWT_SECRET` to the env contract. Never log or expose the raw token.
