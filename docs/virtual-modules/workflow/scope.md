# `zigttp:scope`

Structured lifecycle management. `scope(name, fn)` opens a lexical
scope; `using(resource, cleanup)` binds a resource to the current
scope; `ensure(fn)` registers a cleanup that runs at scope exit.
Cleanups run in LIFO order whether the scope returns normally or
propagates a Result error.

## Summary

```ts
import { scope, using, ensure } from "zigttp:scope";

function handler(req) {
  return scope("request", () => {
    const conn = using(openConnection(), (c) => c.close());
    ensure(() => metrics.requestDone(req.path));

    const result = conn.query("SELECT 1");
    return Response.json({ ok: true, result });
  });
}
```

`openConnection()`'s cleanup runs before `metrics.requestDone()`
(LIFO), regardless of whether the body returns or short-circuits.

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `scope` | `scope(name, fn): unknown` | write | Open a named scope; run `fn`; run every registered cleanup in LIFO order. |
| `using` | `using(resource, cleanup): resource` | write | Bind `cleanup` to the enclosing scope; return the resource unchanged. |
| `ensure` | `ensure(fn): undefined` | write | Register a cleanup to run at scope exit. |

Scopes can nest. A `using` or `ensure` call inside a nested scope
binds to the innermost scope.

## Compile-time proof

- Every literal `scope(name, fn)` records the name in the `scope`
  contract section. `scope.maxDepth` captures the deepest proven
  nesting level.
- Handlers that use `scope` gain the `state_isolated` property
  because cleanups guarantee no resource leaks between requests.
- `scope`, `using`, `ensure` are `traceable = false` - they do not
  appear in `BehaviorPath.io_sequence`.

## Runtime failures

- If a cleanup throws, the scope still runs the remaining cleanups;
  the first error is rethrown after the rest finish.
- `using()` and `ensure()` called outside a scope throw with a
  "no active scope" message.

## Related

- [`zigttp:durable`](./durable.md) - for work that must survive a
  crash, not just a scope exit.
- [`zigttp:io`](./io.md) - `parallel()` runs its branches in their
  own scopes so a failing branch doesn't leak resources from
  succeeding ones.
