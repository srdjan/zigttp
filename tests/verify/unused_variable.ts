// Should warn: declared variable is never used
function handler(req: Request): Response {
    const unused = "hello";
    return Response.json({ ok: true });
}
