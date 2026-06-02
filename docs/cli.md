# CLI Reference

The developer surface is a single binary: `zigttp`. The standalone
`zigts` binary is also installed for IDE and CI integrations that prefer
to call the analyzer directly; every `zigts <command>` is also reachable
as `zigttp <command>`. The serverless runtime template ships as
`zigttp-runtime` and is invoked automatically by `zigttp` itself — you
should never need to type its name.

Every command keeps its own `--help`. This page is the index.

## Developer commands

Day-to-day use is five commands. Everything else is advanced and stays
out of the default help; run `zigttp help --all` for the full list.

```bash
zigttp init <name> [--template basic|api|htmx]
zigttp dev [options] [handler.ts]
zigttp test [tests.jsonl]
zigttp expert
zigttp deploy [--no-attest]
```

Project commands auto-detect `zigttp.json` from the current directory or
a parent. Scaffolded projects bind port `3000`; raw `serve` defaults to
`8080` unless a project manifest is discovered.

### `zigttp demo`

Runs the local Proof Theater. The scripted form is the release-friendly path:

```bash
zigttp demo --scripted --out proof-demo --export proof-demo/passport
```

It creates a demo workspace, applies the unsafe secret-flow edit, repairs it,
deploys locally, and writes an offline Proof Passport with `passport.json`,
`events.jsonl`, `session-meta.json`, `verify.txt`, and `index.html`.

### Common `dev` and `serve` options

```text
-p, --port <PORT>       Port (project default: 3000; raw serve: 8080)
-h, --host <HOST>       Host (default: 127.0.0.1)
-e, --eval <CODE>       Inline JavaScript handler
-m, --memory <SIZE>     JS runtime memory limit (default: 0 = no limit)
-n, --pool <N>          Runtime pool size (default: auto)
-q, --quiet             Disable request logging
    --cors              Enable CORS headers
    --static <DIR>      Serve static files
    --outbound-http     Enable native outbound bridge (fetchSync/httpRequest)
    --outbound-host <H> Restrict outbound bridge to exact host H
    --outbound-timeout-ms <N>
    --outbound-max-response <SIZE>
    --watch             Watch handler files and hot-swap on change
    --prove             With --watch: diff behavioral contracts before swapping
    --force-swap        With --watch --prove: apply breaking changes anyway
    --trace <FILE>      Record handler I/O traces to JSONL file
    --record-proof      (dev only) Capture this session into a replayable proof
                        capsule at .zigttp/capsules/default/; replay later with
                        `zigttp proof replay default`
    --replay <FILE>     Replay recorded traces and verify handler output
    --test <FILE>       Run declarative handler tests from JSONL file
    --sqlite <FILE>     SQLite database path for zigttp:sql
    --durable <DIR>     Enable durable execution with write-ahead oplog
    --system <FILE>     System registry for zigttp:service
    --security-log <FILE>
    --lifecycle <MODE>  Runtime lifecycle: ephemeral | bounded | reuse
    --no-env-check      Skip startup env var validation (development use)
```

### `zigttp deploy`

Verifies the handler in the current project, emits a self-contained
binary at `.zigttp/deploy/<project-name>`, and appends a `kind=deploy`
row to `.zigttp/proofs.jsonl`. No cloud credentials, no Docker, no
network access.

```bash
zigttp deploy
./.zigttp/deploy/<project-name>
```

`zigttp deploy --local` and `zigttp deploy --target local` are explicit
aliases for the same path.

Attestation is default-on: the build signs a JWS that the running server
emits on every response as `Zigttp-Attest` and serves at
`GET /.well-known/zigttp-attest` as JSON. The signing key is the
persistent identity at `~/.zigttp/attest/keypair.bin`. Pass `--no-attest`
to skip signing for one build. Full flow: [User Guide - Proof ledger and badge](user-guide.md#proof-ledger-and-badge).

Hosted cloud deploy is out of core for v0.1.0-beta. `zigttp deploy --cloud`
and the related account commands reject with a "not in this beta" message.

### `zigttp edge`

In-process edge runtime that loads multiple handler pools and routes
incoming requests by host, method, and path prefix to a named target.
Useful for multitenant edge, A/B routing, or internal request fan-out.

```bash
zigttp edge --config zigttp.edge.json
```

Config is JSON with three sections (listener, handlers, routes). Full
reference: [edge.md](edge.md).

### `zigttp proofs`

Browse the local proof ledger at `.zigttp/proofs.jsonl`. `zigttp deploy`
and `dev --watch --prove` swaps append rows; `proofs` itself never
writes. Each row carries a verdict (`safe`, `safe_with_additions`,
`breaking`) computed against the prior row.

```bash
zigttp proofs                              # last 10 entries, newest last
zigttp proofs show <ref>                   # re-render the review card
zigttp proofs diff <a> <b>                 # render b with a as baseline
zigttp proofs watch                        # tail new entries (Ctrl+C to exit)
zigttp proofs export [--format md|html|svg] [--ref HEAD]
zigttp proofs badge [--out PATH] [--inline] [--public-url URL] [--ref HEAD]  # SVG verdict badge
zigttp proofs bundle --contract PATH --out DIR [--binary PATH] [--replay PATH]
zigttp proofs verify <bundle-dir>          # re-check every sha256
zigttp proofs gate [--base REF] [--head REF] [--format md|json]  # PR gate; exit 1 on breaking
```

Refs may be `HEAD`, `HEAD~N`, or a contract sha prefix. The ledger
persists only contract-derived identifiers; no env values, tokens, or
PII.

`zigttp proofs gate` is the pull-request gate rather than a ledger view:
it compiles the before and after of every handler changed in a git range,
aggregates one repo-level behavioral verdict (`equivalent` /
`equivalent_modulo_laws` / `additive` / `breaking`), and emits a PR-ready
Markdown report or machine JSON. Exit `0` safe, `1` breaking, `2` git or
usage error. See [proof-gate.md](proof-gate.md) for the shipped GitHub
Action that posts it as a sticky comment.

### `zigttp proof`

Replay a recorded proof capsule (under `.zigttp/capsules/<name>/`)
against the current handler. A capsule bundles a handler's recorded
request traces with a manifest pinning the handler, contract, and policy
hashes, so `proof replay` answers "did this edit change behavior on a
request I actually exercised?"

```bash
zigttp proof replay <capsule>                     # exit 0 = reproduced, 1 = regression
zigttp proof replay <capsule> --allow-version-mismatch
```

Each recorded request is replayed against replay stubs (no live
effects); a status or body change counts as a regression and exits 1.
Replay fails closed when the capsule's schema or policy hash no longer
matches the running binary - the recorded behavior was proven under a
different rule set - and `--allow-version-mismatch` overrides that gate.
The capsule format lives in `packages/runtime/src/capsule.zig`. Distinct
from `zigttp proofs` (the ledger viewer above): `proof` replays traces,
`proofs` renders receipts.

### `zigttp verify`

Third-party proof-receipt verifier. Fetches a deployed URL, reads its
`Zigttp-Attest` response header, validates the embedded Ed25519
signature, and prints the proven claims.

```bash
zigttp verify <url>                           # human-readable claims
zigttp verify <url> --json                    # one-line JSON for CI
zigttp verify <url> --trust-key <hex>         # pin an expected fingerprint
```

Exit codes: `0` verified, `1` argument error, `2` endpoint missing the
header, `3` signature invalid, `4` fingerprint mismatch, `5` HTTP error.

`zigttp verify <url>` is distinct from `zigttp proofs verify
<bundle-dir>`: the first checks a live endpoint over HTTP, the second
re-hashes the components of a locally-stored proof bundle.

### `zigttp doctor`

Reports environment readiness: platform, manifest validity, entry-file
presence, path permissions, and analyzer pass status.

### `zigttp studio`

Optional browser proof workbench for a handler. It runs the same watch and
proof loop as `zigttp dev`, then mirrors the terminal proof card at
`/_zigttp/studio`.

```bash
zigttp studio [handler.ts]
zigttp dev --studio
```

Studio keeps the terminal workflow as the source of truth: a failed save keeps
the previous handler running, and the browser state follows the same analyzer
verdict.

## Machine tools

Standalone analysis and compilation without starting a server. All of
these are also available as `zigts <command>` for IDE and CI tooling
that prefers calling the analyzer binary directly; the surface and
output formats are identical.

```bash
zigttp check [handler.ts] [options]
zigttp compile [--system path] <handler.ts> <out.zig>
zigttp prove <old.json> <new.json>             # exit 0=safe, 1=breaking, 2=needs_review
zigttp prove-behavior <before.ts> <after.ts> [--json]  # behavioral verdict; exit 0=safe, 1=breaking, 2=error
zigttp mock <tests.jsonl> [--port N]           # mock server from test cases
zigttp link <system.json>                      # cross-handler contract linking
zigttp rollout <old-system.json> <new-system.json>
zigttp features [--json]                       # allowed/blocked language features
zigttp modules [--json]                        # virtual modules and exports
zigttp restrictions [--json] [--by proof|class]
zigttp meta [--json]                           # policy metadata (version, hash, rule count)
zigttp gen-tests [handler.ts] [-o output.jsonl]
zigttp verify-paths <f>... [--json]            # full analysis (includes flow-checker)
zigttp verify-modules --builtins --strict --json
zigttp verify-module-manifest <manifest.json> [--json]  # validate a partner module manifest
zigttp extension-status --module-manifest <path>... [--json]  # registered partner-module summary
zigttp edit-simulate [handler.ts] [--before old.ts]
zigttp canonicalize <file> --json [--simulate] # local canonical refactor intents
zigttp normalize <file> [--write] [--check] [--json]  # rewrite into Canonical Normal Form
zigttp describe-rule [name|code] [--json] [--hash]
zigttp search <keyword> [--json]
zigttp review-patch <file> [--before <old>] [--diff-only]
zigttp ratchet show|check <handler.ts>
zigttp witnesses ...
```

### Structured JSON output

`zigttp check --json handler.ts` writes machine-readable diagnostics to
stdout. Add `--system system.json` when the handler uses `serviceCall()`
and you want compile-time typing for internal service responses.

```json
{
  "success": false,
  "diagnostics": [{
    "code": "ZTS001",
    "severity": "error",
    "message": "'try/catch' is not supported",
    "file": "handler.ts",
    "line": 23,
    "column": 3,
    "suggestion": "use Result types for error handling"
  }]
}
```

Code ranges: ZTS0xx parser, ZTS1xx sound mode, ZTS2xx type checker,
ZTS3xx handler verifier, ZTS4xx flow checker, ZTS5xx author-declared
specs, ZTS6xx strict ZigTS profile and one-way canonical diagnostics.

`zigttp normalize --json` includes `fullyCanonical`, `rewriteTrace`,
`residual`, and `residualDiagnostics` so CI and agents can distinguish
successful rewrites from remaining canonical blockers.

## `zigttp auth` provider keys

Stores API keys for the LLM providers `zigttp expert` and handler-side
`zigttp:env` lookups consume, so users do not have to export shell
variables or maintain a `.env` file.

```bash
zigttp auth claude              # prompt (hidden input) for an Anthropic key
zigttp auth openai              # same for OpenAI
zigttp auth status              # show configured providers, masked
zigttp auth revoke <provider>   # delete a stored key (claude | openai)
```

Storage is `~/.zigttp/providers.json` at mode 0600, sibling to the
`~/.zigttp/credentials` file used by hosted-cloud deploy. The runtime
injects stored values into `ANTHROPIC_API_KEY` / `OPENAI_API_KEY` at the
start of `zigttp dev`, `zigttp serve`, and `zigttp expert`. A shell-set
variable always wins; the file only fills the gap when the shell value
is unset or blank.

## `zigttp expert` interactive agent

Compiler-in-the-loop coding agent. Picks its model backend from the
environment: `ANTHROPIC_API_KEY` selects the Anthropic provider,
`OPENAI_API_KEY` selects the OpenAI provider. If neither variable is
set to a non-empty value, the CLI exits with a setup message. The
fastest setup path is `zigttp auth claude`, which stores a key and
auto-injects it on launch; exporting the variable in your shell works
too.

The persona, reference material, skill catalog, prompt templates,
and compiler metadata are baked into the binary. The one
external input that reaches the system prompt is `AGENTS.md` /
`CLAUDE.md`, walked up from cwd to the enclosing `.git/` directory and
appended as a labelled read-only project-context section with a 128 KiB
cap. Disable with `--no-context-files`.

Full pi architecture: [../packages/pi/README.md](../packages/pi/README.md).

### Modes

```bash
# Interactive REPL
zigttp expert
zigttp expert --resume                    # resume newest session for this cwd
zigttp expert --continue                  # alias for --resume
zigttp expert --session-id <id>           # named or resumed session
zigttp expert --fork <session-id>         # branch from an existing session
zigttp expert --yes                       # auto-approve all verified edits
zigttp expert --no-edit                   # auto-reject all verified edits
zigttp expert --no-context-files          # skip AGENTS.md / CLAUDE.md load
zigttp expert --no-perf-receipt           # skip the signed perf receipt on applied edits
zigttp expert --tools minimal             # workspace-read-only tool preset
zigttp expert --tools full                # full compiler tool preset (default)

# Non-interactive (one turn and exit)
zigttp expert --print "add a GET /health route"
zigttp expert --print "..." --mode json   # NDJSON event stream to stdout

# Line-delimited JSON-RPC 2.0 over stdio (long-lived session)
zigttp expert --mode rpc

# Property-goal autoloop (compiler-driven; LLM is not in the loop)
zigttp expert --handler handler.ts --goal no_secret_leakage,injection_safe
zigttp expert --handler handler.ts --goal no_secret_leakage --max-iters 4
```

In `--mode json`, each event is `{"v":1,"k":"<kind>","d":<payload>}`.
Kinds: `user_text`, `model_text`, `tool_use`, `tool_result`,
`proof_card`, `diagnostic_box`, `verified_patch`, `system_note`,
`autoloop_outcome`, `end`. Persisted `events.jsonl` lines use the same
envelope; `end` is live-stream-only.

In `--mode rpc`, each stdin line is a JSON-RPC 2.0 request. Methods:
`turn`, `compact`, `session.info`, `tools.list`, `tools.invoke`,
`skills.list`, `templates.{list,expand}`, `model.{list,set}`,
`shutdown`. During a `turn`, each new transcript entry emits as a
notification with method `"event"` before the final result.

### Interactive REPL slash commands

```text
/compact                       collapse the session transcript into a summary
/fork                          branch the current session into a new directory
/tree                          list all sessions for this workspace
/resume  /continue             reload the newest session for this cwd
/new                           start a fresh session
/model                         show the active model and available IDs
/model <id>                    switch to a different model mid-session
/skills                        list available skill shortcuts
/skill:<name>                  send the named skill body as a prompt
/templates                     list available prompt templates
/template:<name> [args...]     expand a template and send it as a prompt
/settings                      show compile-time defaults (model, token limits)
/studio <handler.ts>           show the browser proof workbench command
/feature route file=<handler.ts> method=<VERB> path=</path>
/forge route file=<handler.ts> method=<VERB> path=</path>
/forge spec file=<handler.ts> specs=<csv> [effects=<csv>]
/hotkeys                       list keyboard shortcuts
/changelog                     recent expert subsystem additions
```

After each model turn the REPL prints cumulative token use:
`[tokens: in=N cache_r=N cache_w=N out=N]`. Totals reset on `/new` or
`/resume`.

### Route Forge

Compiler-native path for adding handler routes. `/feature` never writes;
`/forge` returns an inspectable candidate and marks it ready only when
it introduces zero new compiler violations.

```bash
/feature route file=handler.ts method=GET path=/health
/forge route file=handler.ts method=POST path=/todos body=todo status=201
```

Apply a ready forge result by asking the expert to use the apply tool or by
running the corresponding apply command from the REPL. The apply path reruns
the compiler veto and records the accepted change as a `verified_patch`.

### Spec Forge

Compiler-native path for proof intent. `/forge spec` adds source-level
`Spec<...>` and optional `Effects<...>` return markers, applies narrow
compiler-owned repairs where supported, and returns a candidate only after
the compiler veto has run.

```bash
/forge spec file=handler.ts specs=deterministic,idempotent
/forge spec file=handler.ts specs=no_secret_leakage,injection_safe effects=env
```

The first repair lane handles `Date.now()` / `Math.random()` by moving the
call into `step(...)` from `zigttp:durable`, which lets the deterministic
and idempotent proofs compose through durable replay. Unsupported structural
repairs are reported as blockers instead of being silently rewritten.

### Property-goal autoloop

`--goal <csv> --handler <path>` short-circuits the conversational run.
The compiler drives a fixed `pi_goal_check → pi_repair_plan →
pi_apply_repair_plan` cycle until every requested property flips green
or a budget trips. The LLM is not in the loop; only the compiler.

Verdicts: `achieved`, `exhausted_iters`, `exhausted_time`, `stalled`,
`regression_blocked`, `tool_failed`. Supported goals are the five
flow-oriented PropertyTags: `no_secret_leakage`, `no_credential_leakage`,
`input_validated`, `pii_contained`, `injection_safe`.

### Proof Delta Card and witnesses

Every accepted edit is persisted as a `verified_patch` event. Inspect
session history with the REPL's `/ledger export <path>` command or the
top-level `zigttp ledger` CLI. The exported ledger carries the same proof
delta data: property changes, violations before vs after, witness counts,
and the patch chain header.

Witness commands stay CLI-driven: use `/witnesses <handler.ts>` to inspect
counterexamples and `zigttp mock --replay` or generated witness regression
files to replay them.

On resume, the session's `meta.json` carries the `policy_hash` it was
created under. If the current binary's hash differs, the REPL prepends
a `[policy drift]` system note so the model knows prior rule citations
may be stale.

Full counterexample reference: [witnesses.md](witnesses.md),
[counterexamples.md](counterexamples.md).
