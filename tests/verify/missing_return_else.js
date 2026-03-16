// Should fail: if-without-else doesn't guarantee a return
function handler(req) {
    const url = req.url;
    if (url === "/health") {
        return Response.json({ status: "ok" });
    }
    // Missing: return for non-health paths
}
