# `zigttp:validate`

JSON Schema validation with a module-scoped schema registry. Compile
schemas once at module load, then validate request bodies against
named schemas per request.

## Summary

```ts
import { schemaCompile, validateJson } from "zigttp:validate";

schemaCompile("user", JSON.stringify({
  type: "object",
  required: ["name", "email"],
  properties: {
    name: { type: "string", minLength: 1, maxLength: 100 },
    email: { type: "string", format: "email" },
    age: { type: "integer", minimum: 0, maximum: 200 },
  },
}));

function handler(req) {
  if (req.method !== "POST") return Response.json({ ok: true });

  const result = validateJson("user", req.body ?? "{}");
  if (!result.ok) {
    return Response.json({ errors: result.errors }, { status: 400 });
  }
  return Response.json({ user: result.value }, { status: 201 });
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `schemaCompile` | `schemaCompile(name, schemaJson): boolean` | boolean | Register a schema under `name`. Called at module scope. |
| `validateJson` | `validateJson(name, json): Result<value, errors>` | Result | Parse and validate a JSON string body. |
| `validateObject` | `validateObject(name, jsonString): Result<value, errors>` | Result | Validate a pre-parsed object passed as JSON. |
| `coerceJson` | `coerceJson(name, json): Result<value, errors>` | Result | Validate and coerce string-shaped numbers and booleans to their schema types. |
| `schemaDrop` | `schemaDrop(name): boolean` | boolean | Remove a schema from the registry. |

Supported formats for `type: "string"`: `email`, `uuid`, `iso-date`,
`iso-datetime`.

## Compile-time proof

- Every literal `schemaCompile(name, ...)` registers `name` in the
  `schemas` contract section; `validateJson(name, ...)` sites that
  reference a known name get payload-aware types at `check --json`
  time.
- `validateJson` labels the returned value as `validated`, which
  the flow checker treats as a sanitization barrier for the
  `user_input` label.

## Runtime failures

- Unknown schema names return `{ ok: false, errors: [...] }` with a
  structured error list.
- Malformed JSON in `validateJson` yields a parse error in the
  Result; it never throws.
- `schemaCompile()` throws synchronously on invalid JSON Schema
  (bad `type`, invalid `format`).

## Related

- [`zigttp:decode`](./decode.md) - schema-backed ingress helpers
  (`decodeJson`, `decodeForm`, `decodeQuery`) built on top of
  this registry.
- [`zigttp:auth`](./auth.md) - JWT claim payloads pair well with a
  schema for the expected claim shape.
