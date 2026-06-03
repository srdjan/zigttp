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
{ "v": 1, "k": "assistant_delta", "d": { "text": "..." } }
```

The event envelope uses:

| Field | Meaning |
|---|---|
| `v` | Event schema version. |
| `k` | Event kind. |
| `d` | Kind-specific payload. |

`zigttp expert --mode rpc` exposes a line-delimited JSON-RPC 2.0 interface over
stdio for long-lived clients. The agent loads its model backend from
`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `~/.zigttp/providers.json`; missing
configuration exits with setup guidance.

## Compatibility

- Additive JSON fields are allowed.
- Removing or renaming documented fields requires a contract version bump.
- Unknown event kinds must be ignored by clients that do not understand them.
- In-process expert tools must round-trip through the same JSON shapes exposed
  by the CLI commands.
