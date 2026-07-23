---
name: auth-jwt
description: Add JWT verification to a handler using zttp:auth.
---
Add JWT auth to the handler:
1. Call `zts_expert_modules` first and confirm the live `zttp:auth` and `zttp:env` exports.
2. Import only what is needed: usually `parseBearer`, `jwtVerify`, and `env`.
3. Read the configured secret with `env("JWT_SECRET")` and return a 500 setup error if it is `undefined`. Never invent a fallback secret.
4. Extract the bearer token from the authorization header and return 401 when it is `undefined`.
5. Call `jwtVerify(token, secret)`. It returns a `Result`, so check `.ok` before reading `.value`.
6. Verify the edit with `pi_goal_check` or the proof card for `no_credential_leakage` and `no_secret_leakage`.

Never log or expose the raw token, secret, or full claims payload. Return only the minimum subject/authorization outcome the handler needs.
