# CLI Reference

`zttp` is the developer CLI. `zts` is installed for tools that want the
analyzer directly; analyzer commands exposed by `zts` are also reachable as
`zttp <command>`.

Run command-specific help for exact flags:

```bash
zttp --help
zttp help --all
zttp <command> --help
```

## Core Commands

The default help shows the day-to-day workflow:

```bash
zttp init <name> [--template basic|api|htmx]
zttp dev [handler.ts]
zttp test [tests.jsonl]
zttp expert
zttp deploy
```

Core commands auto-detect `zttp.json` from the current directory or a parent.

| Command | Purpose |
|---|---|
| `init` | Create a project scaffold. |
| `dev` | Run locally, watch files, and prove on save. |
| `test` | Run declarative handler tests. |
| `expert` | Start the compiler-in-the-loop coding agent. |
| `deploy` | Build, prove, attest, and emit a local binary. |

## Run Commands

`zttp serve` runs a handler without the proof-aware watch loop:

```bash
zttp serve src/handler.ts -p 3000
zttp serve -e "function handler(req) { return Response.json({ ok: true }) }"
```

Common `dev` and `serve` flags:

| Flag | Purpose |
|---|---|
| `-p`, `--port <port>` | Listen port. |
| `-h`, `--host <host>` | Listen host. |
| `-e`, `--eval <code>` | Inline handler source. |
| `-m`, `--memory <size>` | Per-runtime JS memory ceiling. |
| `--max-body-size <size>` | Request body limit (default 1m); oversize returns 413. |
| `--max-websocket-connections <count>` | Live WebSocket limit (default 1024; `0` disables upgrades). |
| `-n`, `--pool <count>` | Runtime pool size. |
| `-q`, `--quiet` | Disable request access logging. |
| `--watch` | Watch handler files. |
| `--prove` | Diff contracts before hot-swap when watching. |
| `--trace <file>` | Record request/response traces. |
| `--incident-log <file>` | Append runtime soundness incidents as JSONL. Off by default. |
| `--replay <file>` | Replay recorded traces. |
| `--test <file>` | Run JSONL handler tests. |
| `--sqlite <file>` | SQLite database for `zttp:sql`. |
| `--durable <dir>` | Durable workflow oplog directory. |
| `--system <file>` | Handler bundle manifest: the `zttp:service` HTTP registry and the `zttp:workflow` in-process sub-handler registry. An optional `entry` field names the bundle's single external HTTP entry point, validated by `zttp link`. Workflow startup fails if a local handler path is unreadable. |
| `--workflow-queue` | Persist durable workflow `call`, `follow`, and `fanout` child dispatch through the workflow queue. Requires `--durable <dir>` and `--system <file>`. |
| `--actor-queue` | Enable process-local in-memory mailboxes for `zttp:queue`. |
| `--outbound-http` / `--outbound-host <host>` | Enable outbound HTTP. The host allowlist matches on host only, not port. |
| `--outbound-timeout-ms <ms>` | Outbound connect timeout (default 10000). Implies `--outbound-http`. |
| `--outbound-max-response <size>` | Outbound response body cap (default 1m). Implies `--outbound-http`. |
| `--security-log <file>` | Append security events as JSONL: policy denials, arena audit failures, persistent-string escapes. |
| `--lifecycle <mode>` | Override the contract-derived runtime lifecycle: `ephemeral`, `bounded`, `ttl`, or `reuse`. |
| `--static <dir>` | Serve static files. |
| `--no-env-check` | Skip startup env validation. |

`dev` adds `--no-prove` (watch and reload without contract gating), `--quest` /
`--no-quest` (replay or skip the first-run proof tour), and `--record-proof`,
which captures the session's requests into a replayable proof capsule at
`.zttp/capsules/default/`. `serve --watch` takes `--prove` and `--force-swap`
(apply a breaking swap anyway) instead. Both take `--studio` when the binary was
built with `-Dstudio`.

Without `--lifecycle`, the runtime derives the pool's recycling policy from the
proven contract: `reuse` when the handler is pure, deterministic, and
state-isolated; `ttl` when it is read-only and state-isolated; `bounded`
otherwise. `bounded` recycles a runtime after 64 requests, `ttl` after 30
seconds, and `ephemeral` gives each request a fresh runtime. A hot swap
re-derives the policy unless the override is set.

`--security-log` writes one JSON object per line. A capability denial from a new
gate site is `{"event":"policy_denied","ts":...,"service":...,"action":...,"resource":{"kind":...,"id":...},"reason":...}`;
the per-module kinds (`policy_denied_env`, `policy_denied_cache`,
`policy_denied_sql`, `arena_audit_failure`, `persistent_string_escape`) are
`{"kind":...,"ts":...,"module":...,"detail":...}`. A background thread drains
the event queue and flushes it at shutdown.

Observability: per-request access logging is on by default (method, path,
status, duration, request id; disable with `-q`), and pool/latency metrics are
logged. There is no scrape-able `/metrics` endpoint yet; that is planned for a
later release.

## Deploy And Proof Receipts

```bash
zttp deploy
./.zttp/deploy/<project-name>
zttp verify http://127.0.0.1:8080
```

`deploy` verifies the current project, writes a local binary, appends a
`kind=deploy` row to `.zttp/proofs.jsonl`, and signs an attestation by
default. `--no-attest` disables signing for that build.

Proof ledger commands:

```bash
zttp proofs
zttp proofs show HEAD
zttp proofs diff HEAD~1 HEAD
zttp proofs export --format md --ref HEAD
zttp proofs badge
zttp proofs gate --base origin/main --head HEAD
```

`zttp proof replay <capsule>` replays a capsule recorded by `zttp dev
--record-proof` against the current handler: exit 0 reproduced, 1 regression. It
fails closed when the capsule's pinned handler, contract, or policy hash no
longer matches (`--allow-version-mismatch` overrides). `zttp verify <url>`
verifies a live endpoint's attestation. `zttp proofs verify <bundle-dir>`
re-hashes a local proof bundle.

`zttp verify --json` includes durable workflow receipt fields when a build was
attested with a workflow contract: `durableWorkflowProofLevel`,
`durableWorkflowRetrySafe`, `durableWorkflowIdempotent`, and
`durableWorkflowFaultCovered`.

Workflow queue dead-letter commands:

```bash
zttp workflow-queue list --durable <dir>
zttp workflow-queue show --durable <dir> <item-id>
zttp workflow-queue replay --durable <dir> <item-id>
zttp workflow-queue discard --durable <dir> <item-id>
```

These commands inspect the persisted queue used by `--workflow-queue`; they do
not operate on the in-memory actor queue from `zttp:queue`.

Durable-run dead-letter commands (a sibling surface: these inspect runs that
permanently failed crash recovery, not queued child dispatch):

```bash
zttp durable dead-runs list --durable <dir>
zttp durable dead-runs show --durable <dir> <id>
zttp durable dead-runs replay --durable <dir> <id>
zttp durable dead-runs discard --durable <dir> <id>
```

See [Durable Workflows](durable-workflows.md#durable-run-recovery-and-dead-letters)
for the quarantine/restart/replay/discard semantics.

## Analyzer Commands

These commands are listed by `zttp help --all` from the shared `zts`
command registry:

```bash
zttp check [handler.ts] [--json] [--contract] [--types]
zttp prove <old-contract.json> <new-contract.json>
zttp prove-behavior <before.ts> <after.ts> [--json] [--sql-schema path]
zttp mock <tests.jsonl> [--port <port>]
zttp link <system.json>
zttp rollout <old-system.json> <new-system.json>
zttp edit-simulate [handler.ts] [--before old.ts]
zttp review-patch <file> [--before old.ts] [--json]
zttp gen-tests [handler.ts] [-o output.jsonl]
zttp canonicalize <file> --json
zttp normalize <file> [--write] [--check] [--json]
zttp features [--json]
zttp modules [--json]
zttp restrictions [--json] [--by proof|class]
zttp meta [--json]
zttp describe-rule [name|code] [--json] [--hash]
zttp search <keyword> [--json]
zttp spec-check [--json]
zttp spec-hash [--json]
zttp spec-render [--out path] [--check path]
zttp verify-paths <file>... [--json]
zttp verify-modules <file>... [--strict] [--json]
zttp verify-modules --builtins --strict --json
zttp verify-module-manifest <manifest.json> [--json]
zttp extension-status --module-manifest <path>... [--json]
```

Use JSON mode for IDEs, CI, and review-bot integrations.

Exit codes for gating: `check` returns 0 (ok), 1 (errors), or 2 (warnings only, no errors). `prove` and `prove-behavior` return 0 (safe), 1 (breaking), or 2 (usage or error). `spec-check` validates the semantics registry against the IR/bytecode tables and returns 0 (conform), 1 (divergence, with a `ZTS75x` counterexample), or 2 (error); `spec-hash` prints the registry hash for CI assertions, the way `describe-rule --hash` prints the policy hash. `spec-render --check <path>` returns 0 when the committed readable spec matches the registry, or 1 when it is stale.

## Expert Mode

Configure a model key, then launch the interactive agent:

```bash
zttp auth claude
zttp auth openai
zttp auth status
zttp auth revoke claude
zttp expert
```

Useful modes:

```bash
zttp expert --resume
zttp expert --yes
zttp expert --no-edit
zttp expert --model claude-sonnet-4-6
zttp expert --print "add a GET /health route"
zttp expert --print "..." --mode json
zttp expert --mode rpc
zttp expert --handler src/handler.ts --goal no_secret_leakage
```

| Flag | Purpose |
|---|---|
| `--resume` | Continue the last session for the current project. |
| `--yes` | Apply every verified edit without prompting. |
| `--no-edit` | Let the model read and analyze files but block all writes. |
| `--model <id>` | Start on a model registered for the configured provider. |
| `--print <text>` | Non-interactive: send one message, print the response, and exit. |
| `--mode json` | Emit JSON-encoded turn events to stdout (pairs with `--print`). |
| `--mode rpc` | Run in RPC mode for editor integrations. |
| `--handler <file>` | Override the handler file (default: auto-detected from `zttp.json`). |
| `--goal <property>` | Restrict the session to edits that achieve a named proof property. |

Pi uses `claude-sonnet-4-6` for Anthropic and `gpt-4o-mini` for OpenAI.
Anthropic remains the measured path; OpenAI support ships as an experimental
Responses API backend. If both credentials are configured, Anthropic takes
precedence.

`--model <id>` accepts an exact ID from the static registry, then checks it
against the provider selected from credentials. A model ID never switches the
provider. `/model` lists only models for the active provider, marks the current
one, and changes the current session when you select another. Selection also
applies that model's request budget: the Claude entries keep their existing
budgets, while `gpt-4o-mini` requests at most 8,192 output tokens despite its
16,384-token output capability. RPC clients get the same allowed set through
`model.list` and the same validation through `model.set`.

The stored provider file is `~/.zttp/providers.json` with mode `0600`.
Environment variables `ANTHROPIC_API_KEY` and `OPENAI_API_KEY` override stored
values.

## Optional Surfaces

- `zttp studio` runs the browser proof workbench when built with `-Dstudio`.
- `zttp edge --config zttp.edge.json` runs the in-process multi-handler
  edge router when built with `-Dedge`.
- `zttp demo --scripted --out proof-demo --export proof-demo/passport`
  creates an offline Proof Passport demo.

See [User Guide](user-guide.md) for the normal project flow.
