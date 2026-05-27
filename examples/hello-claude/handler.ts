// A tiny handler used as the canvas for `zigttp expert`. The interactive
// agent reads this file, streams Claude's responses into your terminal as
// it works, and (with your approval) modifies the file in place.
//
// Walk through this with examples/hello-claude/README.md.

export function handler(req: Request): Response {
  if (req.path === "/health") {
    return Response.json({ ok: true });
  }
  return Response.text("Hello, Claude.\n");
}
