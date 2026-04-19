# `zigttp:io`

Structured concurrent I/O. `parallel()` waits for every branch to
finish; `race()` returns the first branch to complete. Handlers stay
synchronous and linear; concurrency happens on OS threads.

## Summary

```ts
import { parallel, race } from "zigttp:io";

function handler(req) {
  const [user, orders, inventory] = parallel([
    () => fetchSync("https://users.internal/api/v1/123"),
    () => fetchSync("https://orders.internal/api/v1?user=123"),
    () => fetchSync("https://inventory.internal/api/v1/789"),
  ]);

  return Response.json({
    user: user.json(),
    orders: orders.json(),
    inventory: inventory.json(),
  });
}
```

Three calls at 50 ms each finish in ~50 ms total instead of ~150 ms.
No async, no await, no Promises.

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `parallel` | `parallel(fns: (() => T)[]): T[]` | write | Run every function; block until all finish; return results in the same order. |
| `race` | `race(fns: (() => T)[]): T` | write | Run every function; return the first to finish; cancel the rest. |

Both accept any number of branches. Branches execute on a shared OS
thread pool managed by the runtime.

## Compile-time proof

- Branches count toward the handler's `maxIoDepth` property. A
  handler calling `parallel([f1, f2, f3])` where each branch does
  one `fetchSync` has `maxIoDepth = 3`.
- The verifier ensures each branch returns the same result shape
  expected by the destructuring pattern.

## Runtime failures

- If one branch of `parallel()` throws, the rest finish, then the
  first error rethrows into handler code.
- `race()` throws if every branch throws; the last error wins.
- Passing a non-function element throws synchronously with a
  TypeError.

## Requirements

- The runtime thread pool is always available.
- The thread pool is shared across handlers in the same process.
  Very large fan-out can starve other requests; keep branch counts
  bounded.

## Related

- [`zigttp:fetch`](../net/fetch.md) - the usual workload inside
  a `parallel` branch.
- [`zigttp:scope`](./scope.md) - cleanups registered inside a
  parallel branch run at that branch's exit, not the enclosing
  scope's.
- [`zigttp:durable`](./durable.md) - for long-running work, durable
  runs are the right primitive; `parallel` is for within-request
  fan-out.
