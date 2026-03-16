// Should pass: all paths return
function handler(req) {
    const url = req.url;
    if (url === "/health") {
        return Response.json({ status: "ok" });
    }
    return Response.json({ error: "not found" }, { status: 404 });
}
