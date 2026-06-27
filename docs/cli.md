# CLI Reference

`zigttp` is the developer CLI. `zigts` is installed for tools that want the
analyzer directly; analyzer commands exposed by `zigts` are also reachable as
`zigttp <command>`.

Run command-specific help for exact flags:

```bash
zigttp --help
zigttp help --all
zigttp <command> --help
```

## Core Commands

The default help shows the day-to-day workflow:

```bash
zigttp init <name> [--template basic|api|htmx]
zigttp dev [handler.ts]
zigttp test [tests.jsonl]
zigttp expert
zigttp deploy
```

Core commands auto-detect `zigttp.json` from the current directory or a parent.

| Command | Purpose |
|---|---|
| `init` | Create a project scaffold. |
| `dev` | Run locally, watch files, and prove on save. |
| `test` | Run declarative handler tests. |
| `expert` | Start the compiler-in-the-loop coding agent. |
| `deploy` | Build, prove, attest, and emit a local binary. |

## Run Commands

`zigttp serve` runs a handler without the proof-aware watch loop:

```bash
zigttp serve src/handler.ts -p 3000
zigttp serve -e "function handler(req) { return Response.json({ ok: true }) }"
```

Common `dev` and `serve` flags:

| Flag | Purpose |
|---|---|
| `-p`, `--port <port>` | Listen port. |
| `-h`, `--host <host>` | Listen host. |
| `-e`, `--eval <code>` | Inline handler source. |
| `-m`, `--memory <size>` | Per-runtime JS memory ceiling. |
| `--max-body-size <size>` | Request body limit (default 1m); oversize returns 413. |
| `-n`, `--pool <count>` | Runtime pool size. |
| `-q`, `--quiet` | Disable request access logging. |
| `--watch` | Watch handler files. |
| `--prove` | Diff contracts before hot-swap when watching. |
| `--trace <file>` | Record request/response traces. |
| `--replay <file>` | Replay recorded traces. |
| `--test <file>` | Run JSONL handler tests. |
| `--sqlite <file>` | SQLite database for `zigttp:sql`. |
| `--durable <dir>` | Durable workflow oplog directory. |
| `--system <file>` | Handler bundle manifest: the `zigttp:service` HTTP registry and the `zigttp:workflow` in-process sub-handler registry. Workflow startup fails if a local handler path is unreadable. |
| `--outbound-http` / `--outbound-host <host>` | Enable outbound HTTP. The host allowlist matches on host only, not port. |
| `--static <dir>` | Serve static files. |
| `--no-env-check` | Skip startup env validation. |

Observability: per-request access logging is on by default (method, path,
status, duration, request id; disable with `-q`), and pool/latency metrics are
logged. There is no scrape-able `/metrics` endpoint yet; that is planned for a
later release.

## Deploy And Proof Receipts

```bash
zigttp deploy
./.zigttp/deploy/<project-name>
zigttp verify http://127.0.0.1:8080
```

`deploy` verifies the current project, writes a local binary, appends a
`kind=deploy` row to `.zigttp/proofs.jsonl`, and signs an attestation by
default. `--no-attest` disables signing for that build.

Proof ledger commands:

```bash
zigttp proofs
zigttp proofs show HEAD
zigttp proofs diff HEAD~1 HEAD
zigttp proofs export --format md --ref HEAD
zigttp proofs badge
zigttp proofs gate --base origin/main --head HEAD
```

`zigttp proof replay <capsule>` replays a recorded proof capsule. `zigttp
verify <url>` verifies a live endpoint's attestation. `zigttp proofs verify
<bundle-dir>` re-hashes a local proof bundle.

## Analyzer Commands

These commands are listed by `zigttp help --all` from the shared `zigts`
command registry:

```bash
zigttp check [handler.ts] [--json] [--contract] [--types]
zigttp prove <old-contract.json> <new-contract.json>
zigttp prove-behavior <before.ts> <after.ts> [--json] [--sql-schema path]
zigttp mock <tests.jsonl> [--port <port>]
zigttp link <system.json>
zigttp rollout <old-system.json> <new-system.json>
zigttp edit-simulate [handler.ts] [--before old.ts]
zigttp review-patch <file> [--before old.ts] [--json]
zigttp canonicalize <file> --json
zigttp normalize <file> [--write] [--check] [--json]
zigttp features [--json]
zigttp modules [--json]
zigttp restrictions [--json] [--by proof|class]
zigttp meta [--json]
zigttp describe-rule [name|code] [--json] [--hash]
zigttp search <keyword> [--json]
zigttp spec-check [--json]
zigttp spec-hash [--json]
zigttp spec-render [--out path] [--check path]
zigttp verify-paths <file>... [--json]
zigttp verify-modules <file>... [--strict] [--json]
zigttp verify-modules --builtins --strict --json
zigttp verify-module-manifest <manifest.json> [--json]
zigttp extension-status --module-manifest <path>... [--json]
```

Use JSON mode for IDEs, CI, and review-bot integrations.

Exit codes for gating: `check` returns 0 (ok), 1 (errors), or 2 (warnings only, no errors). `prove` and `prove-behavior` return 0 (safe), 1 (breaking), or 2 (usage or error). `spec-check` validates the semantics registry against the IR/bytecode tables and returns 0 (conform), 1 (divergence, with a `ZTS75x` counterexample), or 2 (error); `spec-hash` prints the registry hash for CI assertions, the way `describe-rule --hash` prints the policy hash. `spec-render --check <path>` returns 0 when the committed readable spec matches the registry, or 1 when it is stale.

## Expert Mode

Configure a model key, then launch the interactive agent:

```bash
zigttp auth claude
zigttp auth openai
zigttp auth status
zigttp auth revoke claude
zigttp expert
```

Useful modes:

```bash
zigttp expert --resume
zigttp expert --yes
zigttp expert --no-edit
zigttp expert --model claude-sonnet-4-6
zigttp expert --print "add a GET /health route"
zigttp expert --print "..." --mode json
zigttp expert --mode rpc
zigttp expert --handler src/handler.ts --goal no_secret_leakage
```

| Flag | Purpose |
|---|---|
| `--resume` | Continue the last session for the current project. |
| `--yes` | Apply every verified edit without prompting. |
| `--no-edit` | Let the model read and analyze files but block all writes. |
| `--model <id>` | Start on a specific provider model. |
| `--print <text>` | Non-interactive: send one message, print the response, and exit. |
| `--mode json` | Emit JSON-encoded turn events to stdout (pairs with `--print`). |
| `--mode rpc` | Run in RPC mode for editor integrations. |
| `--handler <file>` | Override the handler file (default: auto-detected from `zigttp.json`). |
| `--goal <property>` | Restrict the session to edits that achieve a named proof property. |

`--model <id>` starts the session on a specific provider model; run `/model`
in-session to list the available ids.

The stored provider file is `~/.zigttp/providers.json` with mode `0600`.
Environment variables `ANTHROPIC_API_KEY` and `OPENAI_API_KEY` override stored
values.

## Optional Surfaces

- `zigttp studio` runs the browser proof workbench when built with `-Dstudio`.
- `zigttp edge --config zigttp.edge.json` runs the in-process multi-handler
  edge router when built with `-Dedge`.
- `zigttp demo --scripted --out proof-demo --export proof-demo/passport`
  creates an offline Proof Passport demo.

See [User Guide](user-guide.md) for the normal project flow.
