// Integration example: all virtual modules
// Tests that zigttp:auth, zigttp:validate, and zigttp:cache resolve correctly

import { parseBearer, jwtVerify, jwtSign } from "zigttp:auth";
import { schemaCompile, validateJson, coerceJson } from "zigttp:validate";
import { cacheGet, cacheSet, cacheStats } from "zigttp:cache";
import { sha256 } from "zigttp:crypto";

type RequestBody = {
    name: string;
    age: number;
};

// Compile a schema on startup
const schema_ok = schemaCompile("user", JSON.stringify({
    type: "object",
    required: ["name", "age"],
    properties: {
        name: { type: "string", minLength: 1, maxLength: 100 },
        age: { type: "integer", minimum: 0, maximum: 200 }
    }
}));

function handler(req: Request): Response {
    // Auth: check JWT from Authorization header
    const auth_header = req.headers["authorization"];
    if (auth_header) {
        const token = parseBearer(auth_header);
        if (token) {
            const secret = "my-secret-key";
            const result = jwtVerify(token, secret);
            if (result.ok) {
                // Cache the verified user
                cacheSet("users", sha256(token), JSON.stringify(result.value), 300);
            }
        }
    }

    // Validate request body
    if (req.method === "POST") {
        const validation = validateJson("user", req.body);
        if (!validation.ok) {
            return Response.json({ errors: validation.errors }, { status: 400 });
        }

        // Sign a response token
        const jwt = jwtSign(JSON.stringify({ sub: "user-123", iat: 1700000000 }), "my-secret-key");

        return Response.json({
            user: validation.value,
            token: jwt,
            cache: cacheStats()
        }, { status: 201 });
    }

    return Response.json({
        schema_compiled: schema_ok,
        cache: cacheStats()
    });
}
