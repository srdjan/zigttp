function handler(req: Request): Response & Spec<"deterministic"> {
  return Response.json({ ok: true });
}
