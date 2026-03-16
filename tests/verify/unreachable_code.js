// Should warn: unreachable code after return
function handler(req) {
    return Response.json({ ok: true });
    const x = 42;
}
