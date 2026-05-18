# Gil-Inspired Patterns for zigttp Virtual Modules

## Goal

Adopt `gil`'s high-level ergonomics for zigttp virtual modules so
handlers remain simple and declarative, while low-level Zig/runtime
mechanics stay hidden behind stable module APIs.

Reference style: [`ivanleomk/gil`](https://github.com/ivanleomk/gil)
(`get/post` one-liners, options objects, response helpers,
automatic defaults).

## Core Product Principle

**Virtual modules should feel like small domain SDKs, not runtime
bindings.**

Handler authors should compose intent (`fetch JSON`, `verify token`,
`cache with ttl`) through compact calls, and never manage transport,
buffering, parser wiring, allocator details, or effect plumbing.

## Recommended API Patterns

### 1. One-liner happy path + optional options object

Follow `gil.get(url, .{})` / `gil.post(url, .{ .json = ... })` style.

- Make the first call signature minimal for common use cases.
- Add optional `options` object for advanced controls.
- Keep option names semantic and portable (`timeoutMs`, `headers`,
  `json`, `query`, `ttlSeconds`, `retries`).

Pattern:

```ts
// Happy path
const user = await fetchJson("https://api/users/42")

// Advanced path
const user = await fetchJson("https://api/users/42", {
  headers: { Authorization: `Bearer ${token}` },
  timeoutMs: 1500,
  retries: 2,
})
```

### 2. Typed convenience methods over primitive building blocks

Expose high-level helpers as the primary interface, while still
providing a lower-level escape hatch.

- Preferred: `fetchJson`, `cache.remember`, `auth.requireBearer`.
- Escape hatch: `fetch`, `cache.get/set`, `auth.parseBearer`.

This keeps handlers concise and avoids repeating ceremony at each call
site.

### 3. Response helper object pattern

Mirror `gil` response ergonomics (`raiseForStatus()`, `json(T)`).

- Return rich result objects with common post-processing helpers.
- Provide standardized helpers such as:
  - `ok` / `status`
  - `text()` / `json()`
  - `requireOk()` (throws typed module error)
  - `header(name)`

This centralizes status handling and body decoding behavior.

### 4. Sensible automatic behavior by default

Like `gil` auto-decompression, modules should auto-apply common
safe defaults.

- Decode known formats automatically when requested.
- Normalize headers/casing internally.
- Apply conservative timeouts if none are provided.
- Add explicit opt-outs only where needed.

Avoid forcing handlers to wire repetitive mechanics.

### 5. Error surfaces should be domain-first

Errors exposed to handlers should describe business intent, not Zig
subsystem internals.

- Good: `error.AuthTokenExpired`, `error.CacheMiss`,
  `error.HttpBadStatus`.
- Avoid leaking allocator/network/parser internals in public API names.
- Attach structured metadata (`status`, `code`, `hint`) for logs.

### 6. Literal-friendly config for compiler/runtime analysis

Keep arguments analyzer-friendly so zigts can preserve effect
classification and capability extraction.

- Prefer object literals and string literals for keys/route names.
- Keep module options shallow and schema-like.
- Avoid callback-heavy APIs for core flows.

This maintains proof/sandbox benefits without burdening handler authors.

### 7. Explicit phase separation: build-time registration, runtime use

For modules needing registry/setup (schemas, SQL, services), separate
API into:

- registration phase (declarative, static-ish)
- runtime execution phase (single-call ergonomic helpers)

Example shape:

```ts
// setup
registerQuery("user.byId", "SELECT ... WHERE id = ?")

// handler path
const user = await sql.one("user.byId", [id])
```

### 8. Composable mini-primitives instead of mega-clients

Prefer a few orthogonal functions over a large mutable class client.

- Easier to reason about effects and permission scopes.
- Better tree-shakeability and smaller handler mental model.
- Aligns with existing `zigttp:*` module style.

### 9. Stable naming conventions across modules

Adopt consistent verbs so users transfer knowledge module-to-module.

- Read: `get`, `one`, `many`, `exists`, `parse`
- Write: `set`, `put`, `exec`, `send`, `issue`
- Validate/guard: `require*`, `assert*`, `verify*`

Predictable names reduce docs lookup and onboarding friction.

### 10. Document happy path first, internals second

Each module page should lead with a 10-line "copy/paste" example,
then options, then failure cases.

The Zig implementation details should stay in internals docs, not in
handler-facing examples.

## Suggested `zigttp:*` Application

### `zigttp:fetch`

- Add/standardize convenience wrappers: `fetchJson`, `postJson`.
- Return response object with `requireOk()` and `json()` helpers.
- Keep advanced transport tuning behind optional options fields.

### `zigttp:auth`

- Lead with `requireBearer(req, opts?)` and
  `requireWebhookSignature(req, secret)`.
- Keep algorithm/key parsing internals hidden from default path.

### `zigttp:cache`

- Add `remember(key, ttlSeconds, factory)` for cache-aside happy path.
- Keep `get/set/incr/delete` as explicit primitives.

### `zigttp:validate` + `zigttp:decode`

- Offer single-call ingress pipelines:
  `decodeJson(req, schema)` returning validated typed data.
- Centralize parse/coercion errors into consistent domain errors.

## Anti-Patterns to Avoid

- Exposing handles that require manual lifecycle management in handler
  code.
- Requiring users to manually stitch serializer/parser pipelines for
  common JSON flows.
- Exporting APIs that mirror internal runtime structs 1:1.
- Making users reason about allocator ownership or transport buffers in
  handler code.

## Review Checklist (for new virtual module APIs)

- Does the module have a one-liner happy path?
- Can advanced controls be passed through a single options object?
- Are error names domain-oriented and stable?
- Does the API hide Zig/runtime mechanics from handlers?
- Are effect/capability-relevant arguments literal-friendly?
- Is the first documentation example runnable in under 15 lines?

