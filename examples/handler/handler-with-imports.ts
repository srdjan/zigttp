// Example handler using file imports
import { greet, formatJson } from "./utils.ts";

export function handler(req: Request): Response {
  const name = "world";
  const greeting = greet(name);
  const body = formatJson({ greeting: greeting, status: "ok" });
  return Response.json({ message: greeting });
}
