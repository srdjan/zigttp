// Should pass: if/else both return
function handler(req) {
    const url = req.url;
    if (url === "/health") {
        return Response.json({ status: "ok" });
    } else {
        return Response.json({ error: "not found" }, { status: 404 });
    }
}
