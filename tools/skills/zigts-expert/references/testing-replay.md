# Testing and Replay

## Declarative Testing (JSONL)

Handler tests use JSONL format. Each line is a JSON object with a `type` field. Tests are grouped: a `test` line starts a new case, followed by `request`, zero or more `io` stubs, and `expect`.

Run with `--test tests.jsonl` at runtime or `-Dtest-file=tests.jsonl` at build time.

### Entry Types

| Type | Purpose | Required Fields |
|------|---------|-----------------|
| `test` | Start a new test case | `name` |
| `request` | The HTTP request to send | `method`, `url`, `headers`, `body` |
| `io` | Stub a virtual module call | `seq`, `module`, `fn`, `args`, `result` |
| `expect` | Assert on the response | `status` (plus optional assertions) |

### Request Entry

```jsonl
{"type":"request","method":"GET","url":"/health","headers":{},"body":null}
{"type":"request","method":"POST","url":"/users","headers":{"content-type":"application/json","authorization":"Bearer tok123"},"body":"{\"name\":\"Alice\"}"}
```

Use `null` for absent body (JSON has no `undefined`). Headers use lowercase keys.

### I/O Stubs

Stubs intercept virtual module calls in execution order. The `seq` field (0-indexed) orders multiple stubs within a test case. The `module` and `fn` fields identify which call to intercept. `result` is the return value (use `null` for `undefined` returns).

```jsonl
{"type":"io","seq":0,"module":"env","fn":"env","args":["APP_NAME"],"result":"MyApp"}
{"type":"io","seq":1,"module":"crypto","fn":"sha256","args":["hello"],"result":"2cf24dba..."}
```

Stubbing optional-returning functions: return `null` to simulate "not found" / absent value:

```jsonl
{"type":"io","seq":0,"module":"env","fn":"env","args":["APP_NAME"],"result":null}
```

Stubbing auth functions:

```jsonl
{"type":"io","seq":0,"module":"auth","fn":"parseBearer","args":["Bearer tok"],"result":"tok"}
{"type":"io","seq":0,"module":"auth","fn":"parseBearer","args":[""],"result":null}
```

### Expect Entry

| Field | Type | Description |
|-------|------|-------------|
| `status` | number | Required. Expected HTTP status code |
| `bodyContains` | string | Response body must contain this substring |

```jsonl
{"type":"expect","status":200,"bodyContains":"ok"}
{"type":"expect","status":401,"bodyContains":"unauthorized"}
{"type":"expect","status":302}
```

### Complete Example

A guard-compose handler with CORS preflight, auth, and error cases:

```jsonl
{"type":"test","name":"OPTIONS returns CORS preflight"}
{"type":"request","method":"OPTIONS","url":"/health","headers":{},"body":null}
{"type":"expect","status":204}

{"type":"test","name":"GET without auth returns 401"}
{"type":"request","method":"GET","url":"/health","headers":{},"body":null}
{"type":"io","seq":0,"module":"auth","fn":"parseBearer","args":[""],"result":null}
{"type":"expect","status":401,"bodyContains":"unauthorized"}

{"type":"test","name":"returns app name from env"}
{"type":"request","method":"POST","url":"/","headers":{},"body":"hello world"}
{"type":"io","seq":0,"module":"env","fn":"env","args":["APP_NAME"],"result":"MyApp"}
{"type":"io","seq":1,"module":"crypto","fn":"sha256","args":["hello world"],"result":"b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"}
{"type":"io","seq":2,"module":"crypto","fn":"base64Encode","args":["Hello from MyApp"],"result":"SGVsbG8gZnJvbSBNeUFwcA=="}
{"type":"expect","status":200,"bodyContains":"MyApp"}

{"type":"test","name":"uses default when env is unset"}
{"type":"request","method":"POST","url":"/","headers":{},"body":"test"}
{"type":"io","seq":0,"module":"env","fn":"env","args":["APP_NAME"],"result":null}
{"type":"io","seq":1,"module":"crypto","fn":"sha256","args":["test"],"result":"9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"}
{"type":"io","seq":2,"module":"crypto","fn":"base64Encode","args":["Hello from zigttp"],"result":"SGVsbG8gZnJvbSB6aWd0dHA="}
{"type":"expect","status":200,"bodyContains":"zigttp"}
```

## Deterministic Replay

Record handler I/O with `--trace traces.jsonl`, then verify with `--replay traces.jsonl`. Build-time replay via `-Dreplay=traces.jsonl` fails the build on regressions.

This works because virtual modules are the only I/O boundary - handlers are deterministic functions of (Request, VirtualModuleResponses). A trace captures every virtual module call and its result, then replay feeds those same results back and asserts the handler produces the same Response.

### Workflow

```bash
# Record a trace during development
zig build run -- handler.ts --trace traces.jsonl -p 3000
# (send requests to localhost:3000, then stop the server)

# Replay to verify determinism after code changes
zig build run -- handler.ts --replay traces.jsonl

# Build-time replay (fails build on regression)
zig build -Dhandler=handler.ts -Dreplay=traces.jsonl
```

## CLI Summary

| Command | Purpose |
|---------|---------|
| `--test tests.jsonl` | Run JSONL test file at runtime |
| `-Dtest-file=tests.jsonl` | Run JSONL tests at build time |
| `--trace traces.jsonl` | Record I/O traces during execution |
| `--replay traces.jsonl` | Replay traces and verify determinism |
| `-Dreplay=traces.jsonl` | Build-time replay verification |
| `zig build mock -- tests.jsonl --port 3001` | Mock server from test cases |
