# zigts Structured Tool Contract

This page documents the stable machine-facing analyzer surfaces used by
`zigttp expert`, IDE integrations, and CI. The live command list comes from
`packages/tools/src/zigts_cli.zig`.

## Version Metadata

Use:

```bash
zigts meta --json
zigttp meta --json
```

The metadata includes compiler version, policy version/hash, rule counts, and
feature/module summaries. Clients should compare `policy_hash` when resuming
cached analysis.

## Diagnostics

Machine commands emit diagnostics with this shape:

```json
{
  "code": "ZTS300",
  "severity": "error",
  "message": "all handler paths must return Response",
  "file": "src/handler.ts",
  "line": 12,
  "column": 5,
  "suggestion": "return a Response on this path"
}
```

Code ranges:

| Range | Owner |
|---|---|
| ZTS0xx | Syntax and unsupported feature detection |
| ZTS1xx | TypeScript stripping and module parsing |
| ZTS2xx | Type checking |
| ZTS3xx | Handler verification |
| ZTS4xx | Flow and data-safety checks |
| ZTS5xx | Active spec discharge |
| ZTS6xx | Canonical profile |

## Stable Commands

| Command | Purpose |
|---|---|
| `meta --json` | Compiler and policy metadata. |
| `features --json` | Supported and rejected language feature catalog. |
| `modules --json` | Virtual module exports from the built binary. |
| `restrictions --json` | Language cuts mapped to proof value. |
| `describe-rule [name|code] --json` | Rule detail and fix guidance. |
| `search <keyword> --json` | Rule search. |
| `check <handler.ts> --json` | Analyzer result, proof envelope, diagnostics, and optional contract. |
| `verify-paths <file>... --json` | Path and flow verification. |
| `verify-modules --builtins --strict --json` | Built-in module governance. |
| `verify-module-manifest <manifest.json> --json` | Extension manifest validation. |
| `extension-status --module-manifest <path>... --json` | Extension status summary. |
| `edit-simulate [handler.ts] --stdin-json` | Pre-apply edit simulation. |
| `review-patch <file> --json` | Diff-aware post-edit review. |

Commands such as `prove`, `mock`, `link`, `rollout`, and `compile` are useful
CLI tools, but their text output is not the structured v1 contract.

## Proof Envelope

`check --json` includes:

- `diagnostics`: standard diagnostics.
- `contract`: handler contract when analysis can produce one.
- `proof`: proof-card data, active specs, proof traces, and witness counts.
- `features` and `modules`: optional catalogs on commands that request them.

Clients should ignore unknown additive fields and must not depend on object key
ordering.

## Expert Event Stream

`zigttp expert --print <prompt> --mode json` emits newline-delimited events:

```json
{ "v": 2, "k": "model_text", "d": "..." }
```

The event envelope uses:

| Field | Meaning |
|---|---|
| `v` | Event schema version (currently `2`). |
| `k` | Event kind. |
| `d` | Kind-specific payload (omitted on the terminal `end` event). |

Event kinds and their `d` payloads:

| Kind | `d` payload |
|---|---|
| `user_text` | bare string (the submitted prompt) |
| `model_text` | bare string (assistant prose) |
| `system_note` | bare string (e.g. a policy-drift notice) |
| `tool_use` | object: `{ "id", "name", "args_json" }` |
| `tool_result` | object: `{ "tool_use_id", "tool_name", "ok", "llm_text", "body", "ui_payload"? }` |
| `proof_card` | object: `{ "llm_text", "ui_payload"? }` |
| `diagnostic_box` | object: `{ "llm_text", "ui_payload"? }` |
| `verified_patch` | object: `{ "llm_text", "ui_payload"? }` |
| `autoloop_outcome` | object: `{ "verdict", "iterations", "goals_met", "goals_unmet", ... }` |
| `end` | none (terminal sentinel: `{ "v": 2, "k": "end" }`) |

`zigttp expert --mode rpc` exposes a line-delimited JSON-RPC 2.0 interface over
stdio for long-lived clients. The agent loads its model backend from
`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `~/.zigttp/providers.json`; missing
configuration exits with setup guidance.

Edits in RPC mode are proposed only through the model-mediated `turn` method:
the model emits an `apply_edit`, the compiler veto runs, and a `verified_patch`
event is returned. `apply_edit` is not a directly invocable entry in
`tools.list` / `tools.invoke` (those expose the read-only analyzer tools); a
client cannot apply an unverified edit out of band.

## Compatibility

- Additive JSON fields are allowed.
- Removing or renaming documented fields requires a contract version bump.
- Unknown event kinds must be ignored by clients that do not understand them.
- In-process expert tools must round-trip through the same JSON shapes exposed
  by the CLI commands.
