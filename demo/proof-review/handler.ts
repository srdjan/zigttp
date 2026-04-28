// Minimal handler used as the "run 1" snapshot in the proof review demo.
// Run 2 is staged inline by demo.sh and adds a route, an env read, and an
// egress host so the second proof review card shows a meaningful delta.

function handler(req: Request): Response {
    return Response.json({ ok: true });
}
