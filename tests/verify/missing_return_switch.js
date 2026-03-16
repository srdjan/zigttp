// Should fail: switch without default case
function handler(req) {
    const method = req.method;
    switch (method) {
        case "GET":
            return Response.json({ ok: true });
        case "POST":
            return Response.json({ created: true });
        // Missing: default case
    }
}
