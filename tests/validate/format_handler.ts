// Test handler for format validation (email, uuid, iso-date, iso-datetime)

import { schemaCompile, validateJson } from "zigttp:validate";

schemaCompile("FormatInput", JSON.stringify({
  type: "object",
  properties: {
    email: { type: "string", format: "email" },
    id: { type: "string", format: "uuid" },
    date: { type: "string", format: "iso-date" },
    timestamp: { type: "string", format: "iso-datetime" }
  },
  required: ["email"]
}));

export default (req: Request): Response => {
  const result = validateJson("FormatInput", req.body);
  if (!result.ok) {
    return Response.json({ errors: result.errors }, { status: 400 });
  }
  return Response.json({ valid: true, value: result.value }, { status: 200 });
};
