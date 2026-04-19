# `zigttp:decode`

Schema-backed request ingress for JSON bodies, URL-encoded forms, and
query strings. Wraps `zigttp:validate` with content-type-aware
coercion.

## Summary

```ts
import { schemaCompile } from "zigttp:validate";
import { decodeJson, decodeForm, decodeQuery } from "zigttp:decode";

schemaCompile("order", JSON.stringify({
  type: "object",
  required: ["sku", "qty"],
  properties: {
    sku: { type: "string" },
    qty: { type: "integer", minimum: 1 },
  },
}));

function handler(req) {
  if (req.method === "POST") {
    const result = decodeJson("order", req.body ?? "");
    if (!result.ok) return Response.json({ errors: result.errors }, { status: 400 });
    return Response.json(result.value, { status: 201 });
  }

  const parsed = decodeQuery("order", req.query ?? {});
  if (!parsed.ok) return Response.json({ errors: parsed.errors }, { status: 400 });
  return Response.json(parsed.value);
}
```

## API

| Export | Signature | Returns | Purpose |
|---|---|---|---|
| `decodeJson` | `decodeJson(schema, body): Result<value, errors>` | Result | Parse JSON body, validate against schema. |
| `decodeForm` | `decodeForm(schema, body): Result<value, errors>` | Result | Parse `application/x-www-form-urlencoded` body, coerce per schema, validate. |
| `decodeQuery` | `decodeQuery(schema, query): Result<value, errors>` | Result | Validate a parsed query object (pass `req.query`), coerce types per schema. |

`decodeForm` and `decodeQuery` coerce strings into schema-declared
numbers and booleans before validation. `decodeJson` does no coercion
(JSON already carries types).

## Compile-time proof

- Every literal `decodeJson(schema, ...)` site declares the request
  body schema in the handler contract, populating the OpenAPI
  `requestBody` when emitting API manifests.
- The returned value carries the `validated` flow label; using it
  in egress does not trigger user-input leakage diagnostics.

## Runtime failures

- All three return `{ ok: false, errors }` on schema validation
  failure. Parsing errors are reported as structured validation
  errors, not thrown.
- Unknown schema names throw synchronously.

## Related

- [`zigttp:validate`](./validate.md) - the underlying schema
  registry. Use `validate` when you have a pre-parsed object or
  need lower-level control.
- [`zigttp:url`](../http/url.md) - use `urlSearchParams` when you
  need query parsing without a schema.
