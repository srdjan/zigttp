// Virtual module system example
// Demonstrates import { ... } from "zigttp:*" syntax

import { env } from "zigttp:env";
import { sha256, hmacSha256, base64Encode } from "zigttp:crypto";

type WebhookPayload = {
    event: string;
    data: string;
};

function verifyWebhook(body: string, signature: string): boolean {
    const secret = env("WEBHOOK_SECRET");
    if (!secret) return false;
    const expected = "sha256=" + hmacSha256(secret, body);
    return expected === signature;
}

function handler(req: Request): Response {
    const appName = env("APP_NAME");
    const hash = sha256(req.body);

    return Response.json({
        app: appName,
        bodyHash: hash,
        greeting: base64Encode("Hello from " + (appName ? appName : "zigttp")),
    });
}
