import { env } from "zigttp:env";

export const load = (id: string): Response => Response.text(id);

const parse = (x: number): number => x;

type Loader = (id: string) => Response;
export const typed: Loader = (id: string): Response => Response.text(id);

function handler(req: Request): Response {
  let key = "API_KEY";
  const value = env(key);
  let count = 1;
  const a = parse(1);
  const b = parse(2);
  const items = [1, 2];
  for (let item of items) {
    Response.json({ item });
  }
  return Response.json({ value, count, a, b });
}
