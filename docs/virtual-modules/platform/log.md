# `zigttp:log`

Structured logging to stderr. Every call emits one JSON object per
line with a timestamp, level, message, and caller-provided context.

## Summary

```ts
import { logInfo, logError } from "zigttp:log";

function handler(req) {
  logInfo("request", { method: req.method, path: req.path });

  const user = getUser(req);
  if (user === undefined) {
    logError("no user", { path: req.path });
    return Response.text("unauthorized", { status: 401 });
  }

  return Response.json({ user });
}
```

Output (stderr, one line per call):

```
{"ts":"2026-04-19T12:34:56.789Z","level":"info","msg":"request","method":"GET","path":"/"}
{"ts":"2026-04-19T12:34:56.790Z","level":"error","msg":"no user","path":"/"}
```

## API

| Export | Signature | Effect | Purpose |
|---|---|---|---|
| `logDebug` | `logDebug(message, context?): undefined` | write | Debug-level diagnostic. |
| `logInfo` | `logInfo(message, context?): undefined` | write | Normal operational event. |
| `logWarn` | `logWarn(message, context?): undefined` | write | Recoverable anomaly. |
| `logError` | `logError(message, context?): undefined` | write | Handler-level failure. |

`context` is a plain object; its keys are merged into the emitted
JSON object. Non-JSON values throw at call time.

## Compile-time proof

- No contract extractions. Logging does not declare egress, env, or
  routes.
- All four are `traceable = false`: log calls do not appear in
  replay traces or behavioral paths, so they don't influence
  upgrade verdicts.

## Runtime failures

- Non-string `message` or non-object `context` throws with a
  TypeError.
- stderr write failures are swallowed. Log calls never fail a
  handler.

## Requirements

- Always available. No flags required.
- Output is line-delimited JSON; downstream log shippers (Vector,
  Fluent Bit, CloudWatch agent) parse it directly.

## Related

- [`zigttp:text`](./text.md) - `mask()` before logging a secret-
  shaped value.
- [`zigttp:time`](./time.md) - the `ts` field is in ISO 8601; use
  `parseIso()` for downstream processing.
