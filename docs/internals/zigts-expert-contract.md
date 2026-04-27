# `zigts` v1 — Structured Tool Contract

This document freezes the structured output of the direct `zigts` tool commands as **v1**. Two clients rely on this contract today (`zigts expert` and CI). Every field below is stable within v1. Additive changes are allowed; removals, renames, and semantic changes are not.

The v1 surface is the minimum set of shapes needed to drive a compiler-in-the-loop workflow. It is not everything `zigts` can emit. Anything not listed here is explicitly outside v1 and may change without notice.

## Version identifiers

Three identifiers appear in almost every response:

- `compiler_version` — the embedded zigts compiler version. Currently `"0.16.0"` (see `packages/tools/src/expert_meta.zig`).
- `policy_version` — the rule-set calendar version. Currently `"2026.04.2"` (see `packages/tools/src/expert_meta.zig`).
- `policy_hash` — a content hash over the full rule registry, computed at startup from `zigts.rule_registry.policyHash()`. Stable across runs of the same binary; changes when rules are added, removed, or edited.
- `module_registry_hash` — a content hash over the accepted virtual-module registry metadata. This is additive in v1 and lets clients detect proof metadata changes independently of rule text.

A client that pins to v1 should also pin to a `policy_hash` and re-verify when it changes. The hash is the fastest way to detect that a rule's text or code moved without a version bump.

## Delivery

All commands below print JSON to stdout when invoked with `--json` and a trailing newline. All commands print a textual form without `--json`. Errors not modeled as diagnostics (argument errors, help requests) exit with non-zero and write to stderr or stdout depending on the subcommand — none of that is part of the v1 JSON contract. Only the `--json` outputs are covered.

All JSON documents are a single line followed by `\n`. No pretty-printing. Clients must not rely on formatting; parse with a JSON parser.

Exit codes are part of v1:

- `0` — command succeeded, no error-severity diagnostics.
- `1` — command ran to completion, one or more error-severity diagnostics were produced **or** (for `describe-rule` with a specific name) the rule was not found.
- Any other non-zero — argument error, I/O error, internal failure. Not part of v1 semantics; treat as "unknown failure."

## Core type: `JsonDiagnostic`

Every diagnostic-producing command emits objects conforming to this shape (`packages/tools/src/json_diagnostics.zig:28-36` and `packages/tools/src/json_diagnostics.zig:217-234`):

```json
{
  "code": "ZTS303",
  "severity": "error",
  "message": "Result value used without checking .ok",
  "file": "handler.ts",
  "line": 23,
  "column": 7,
  "suggestion": "check result.ok before accessing result.value"
}
```

Fields:

- `code` (string) — stable diagnostic identifier. See "Diagnostic code ranges" below. v1 guarantees the code string and its meaning do not change for the lifetime of v1.
- `severity` (string) — one of `"error"`, `"warning"`, or `"info"`. v1 guarantees this set is closed; new severities would be a v2 change.
- `message` (string) — human-readable, single-line. Content may be tuned across minor releases.
- `file` (string) — the file the diagnostic applies to. Always present, may be a path or a synthetic name.
- `line` (integer ≥ 0) — 1-based line number. `0` means "file-level, no source location."
- `column` (integer ≥ 0) — 1-based column. `0` allowed with `line == 0` or when the checker has no column.
- `suggestion` (string or `null`) — an actionable fix. Always present; `null` when no suggestion is available. Clients that drive a coding agent should feed this directly back to the model.

Diagnostics carry no trailing comma, no extra fields today, but clients **must ignore unknown fields** so v1 can grow additively.

## Diagnostic code ranges

Frozen in v1 (`packages/tools/src/json_diagnostics.zig:42-129`):

- `ZTS000` — file-level error envelope used by `verify-paths` when a file cannot be read or processed at all. `line` and `column` are `0`, `message` contains the Zig error name.
- `ZTS001`–`ZTS0xx` — parser errors (unsupported feature, unexpected token, invalid import, JSX mismatches, etc.).
- `ZTS100`–`ZTS1xx` — bool-checker diagnostics (non-boolean condition, nullish misuse, arithmetic on non-numeric).
- `ZTS200`–`ZTS2xx` — type-checker diagnostics (type mismatch, arg count, return type, non-exhaustive match).
- `ZTS300`–`ZTS3xx` — handler verifier (missing return path, unchecked result, unused imports, module-scope mutation, unchecked optional access).
- `ZVM001`+ — virtual-module audit diagnostics from `verify-modules` (direct effect usage, missing checked helpers, spec drift). See `packages/tools/src/module_audit.zig`.

New codes may be added in v1. Existing codes cannot move ranges or change meaning.

## `zigts meta`

Emits compiler and policy metadata. Use it to record which policy a session or CI run was verified against.

**Invocation:** `zigts meta --json`

**Output shape** (`packages/tools/src/expert.zig:101-105`):

```json
{
  "compiler_version": "0.16.0",
  "policy_version": "2026.04.2",
  "policy_hash": "<hex>",
  "module_registry_hash": "<hex>",
  "rule_count": 42,
  "categories": {
    "verifier": 11,
    "policy": 20,
    "property": 11
  },
  "mode": "embedded"
}
```

Fields:

- `compiler_version`, `policy_version`, `policy_hash`, `module_registry_hash` — as described above.
- `rule_count` (integer) — number of rules in the registry. Matches `categories.verifier + categories.policy + categories.property`.
- `categories` (object, closed in v1) — rule counts by category. The three keys `verifier`, `policy`, `property` are guaranteed; new categories would be a v2 change.
- `mode` (string) — always `"embedded"` in v1. Reserved for a future out-of-process mode.

**Exit code:** `0` on success.

## `zigts verify-paths`

Runs the full analysis pipeline on one or more handler files. This is the primary "compile everything and tell me what's wrong" call.

**Invocation:** `zigts verify-paths <file>... --json`

**Output shape** (`packages/tools/src/expert.zig:197-212`):

```json
{
  "ok": true,
  "policy_version": "2026.04.2",
  "policy_hash": "<hex>",
  "checked_files": ["handler.ts", "admin.ts"],
  "violations": [ /* JsonDiagnostic, ... */ ]
}
```

Fields:

- `ok` (boolean) — `true` iff no error-severity diagnostic was produced for any file. Warnings do not flip this to `false`.
- `policy_version`, `policy_hash` — pin so the client can correlate violations with a specific rule set.
- `checked_files` (string array) — the files the command attempted to analyze. Order matches the argv order.
- `violations` (array of `JsonDiagnostic`) — all diagnostics across all files, in an unspecified but stable-per-run order. Each diagnostic's `file` field identifies which input it belongs to.

**File-level failure envelope:** if a file cannot be opened or processed, `verify-paths` emits a `ZTS000` diagnostic with `line: 0`, `column: 0`, `message` set to the Zig error name, and `suggestion: null`, then continues with the remaining files (`packages/tools/src/expert.zig:156-172`). `ok` becomes `false`.

**Exit code:** `0` if `ok == true`, `1` if any error-severity diagnostic is present.

## `zigts verify-modules`

Audits zigts built-in virtual module source files and their JSON specs. The authoritative public built-in set comes from `packages/zigts/src/builtin_modules.zig`; editor hooks may still point at individual files under `packages/zigts/src/modules/`, but files outside that public set are ignored so helper edits do not produce governance noise.

**Invocation:**

```text
zigts verify-modules <file>... [--strict] --json
zigts verify-modules --builtins [--strict] --json
```

**Output shape** (`packages/tools/src/expert.zig:271-286`):

```json
{
  "ok": true,
  "policy_version": "2026.04.2",
  "policy_hash": "<hex>",
  "checked_files": ["packages/modules/src/platform/env.zig"],
  "violations": [ /* JsonDiagnostic with ZVM00x codes */ ]
}
```

Identical shape to `verify-paths`. The only differences are the accepted input paths and the diagnostic codes produced (`ZVM00x` series).

Mode semantics:

- Positional-path mode audits the supplied public built-in module/spec files only.
- `--builtins` audits the full authoritative public built-in set and reports every built-in module/spec file in `checked_files`.
- Positional paths and `--builtins` are mutually exclusive.
- `--strict` upgrades governance warnings that must block CI into errors. In v1 that means `ZVM003` ("built-in module has no module spec artifact") becomes error-severity instead of warning-severity.

**Exit code:** `0` if no errors, `1` otherwise.

## `zigts verify-module-manifest`

Validates a proof-carrying virtual module manifest without requiring a Zig backend to be built.

**Invocation:**

```text
zigts verify-module-manifest <manifest.json> --json
```

**Output shape:** identical to `verify-modules`:

```json
{
  "ok": true,
  "policy_version": "2026.04.2",
  "policy_hash": "<hex>",
  "checked_files": ["zigttp-module.json"],
  "violations": [ /* JsonDiagnostic with ZVM00x codes */ ]
}
```

The validator checks schema version, specifier prefix, backend/state model, capabilities, exports, effects, return kinds, labels, contract extraction rules, contract flags, laws, and duplicate export names within the module.

**Exit code:** `0` if no errors, `1` otherwise.

## `zigts edit-simulate`

Simulates running the analysis pipeline on a candidate file and, optionally, diffs the result against a baseline so the client can tell which violations are *new* vs. *pre-existing*. This is the call the TUI's compiler veto will hang from.

**Invocation:**

```
zigts edit-simulate [<handler.ts>] [--before <old.ts>] [--stdin-json]
```

Either a handler path on argv or `--stdin-json` must be supplied. With `--stdin-json`, the command reads the following shape from stdin (`packages/tools/src/edit_simulate.zig:106-134`):

```json
{
  "file": "handler.ts",
  "content": "<full new file contents>",
  "before": "<full old file contents, optional>"
}
```

`file` is a label, not a path on disk — the command writes the content to a temp file under `/tmp/zigts-edit-sim-*` with the same extension as the label and verifies that. This is the key property for the TUI: no file needs to be on disk for the veto to run.

**Output shape** (`packages/tools/src/edit_simulate.zig:137-166`):

```json
{
  "violations": [
    {
      "code": "ZTS303",
      "severity": "error",
      "message": "Result value used without checking .ok",
      "line": 23,
      "column": 7,
      "introduced_by_patch": true,
      "suggestion": "check result.ok before accessing result.value"
    }
  ],
  "summary": {
    "total": 1,
    "new": 1,
    "preexisting": 0
  }
}
```

Fields on each violation:

- `code`, `severity`, `message`, `line`, `column`, `suggestion` — as in `JsonDiagnostic`, except `file` is omitted (edit-simulate is scoped to a single file).
- `introduced_by_patch` (boolean) — `true` if the violation does not appear in the baseline (`before`) analysis. If no baseline was supplied, every violation is `true`.
- `suggestion` — field is **omitted entirely** when there is no suggestion, unlike `JsonDiagnostic` which emits `"suggestion": null`. This difference is part of v1; clients must handle both "absent" and "null." v2 may unify them.

Fields on `summary`:

- `total` (integer) — count of violations reported.
- `new` (integer) — count with `introduced_by_patch: true`.
- `preexisting` (integer) — count with `introduced_by_patch: false`. Zero when no baseline is supplied.

**Violation identity for diffing:** two violations are considered "the same" across the before/after analyses if they share a `(code, message)` pair. Line and column are explicitly ignored so that edits which shift code around are not counted as new violations (`packages/tools/src/edit_simulate.zig:242-252`). This is a v1 heuristic and is documented as such in the source; clients should expect some false positives for pathological cases (identical messages at different locations will be collapsed).

**Exit code:** `0` always from the command itself — the surface is designed for a hook that never wants to fail the tool call. Failure signal is the presence of `summary.new > 0` or `summary.total > 0`, which the client reads.

## `zigts review-patch`

A thin wrapper over `edit-simulate` that adds `--diff-only` for filtering to new violations and has a file-driven path mode. With `--json`, the output shape is **byte-identical** to `edit-simulate`'s output (same `violations`/`summary` envelope).

**Invocation:**

```
zigts review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]
```

`--diff-only` filters `violations` to entries with `introduced_by_patch: true` and rewrites `summary.total` and `summary.new` to the filtered count, `summary.preexisting` to `0` (`packages/tools/src/review_patch.zig:73-88`).

**Exit code:** `0` if `summary.total == 0`, `1` otherwise (`packages/tools/src/review_patch.zig:105`). This is the one place in v1 where a non-zero exit means "the client should surface this," not "the tool broke." Clients running under hooks that should not fail must either catch the exit or use `edit-simulate` directly.

## `zigts describe-rule`

Rule catalog introspection. Lists all rules, a specific rule by name or code, or emits just the `policy_hash` for CI assertions.

**Invocation:**

```
zigts describe-rule [<name-or-code>] [--json] [--hash]
```

**`--hash` output:** a single line containing the hex `policy_hash` followed by `\n`. This is not JSON and has no `--json` variant. It is a v1 primitive for CI scripts that want to pin rule versions. Not part of the JSON contract; treat as a separate format.

**List mode** (`describe-rule --json`, no name): a JSON array of `Rule` objects.

**Single mode** (`describe-rule <name-or-code> --json`): a single `Rule` object, not wrapped in an array.

**`Rule` shape** (`packages/tools/src/describe_rule.zig:81-97`):

```json
{
  "name": "unchecked-result",
  "code": "ZTS303",
  "category": "verifier",
  "description": "Result value used without checking .ok",
  "example": "const r = parse(input); use(r.value);",
  "help": "Check result.ok before accessing result.value. Result types..."
}
```

Fields:

- `name` (string) — stable, kebab-case identifier.
- `code` (string) — matches the `code` field on a `JsonDiagnostic` with the same rule.
- `category` (string) — one of `"verifier"`, `"policy"`, `"property"` in v1.
- `description` (string) — single-line summary.
- `example` (string or absent) — optional short code snippet. **Absent, not `null`**, when the rule has no example.
- `help` (string) — multi-paragraph prose explaining why the rule exists and how to fix violations. Always present.

**Exit code:** `0` on list, `0` when a named rule is found, `1` when a named rule is not found (`packages/tools/src/describe_rule.zig:46`).

## `zigts search`

Substring search across rule names, descriptions, and help text.

**Invocation:** `zigts search <keyword> [--json]`

**Output** (`packages/tools/src/search_rules.zig:36-44`): a JSON array of `Rule` objects matching the keyword, using the exact shape from `describe-rule`. An empty array is a valid result and is returned as `[]`.

**Exit code:** `0` always.

## Proof-card envelope (from `zigts check --json`)

`zigts check --json` is not reached via the direct verification commands, but it is part of the v1 contract because `zigts expert` calls the same underlying functions in-process. Two envelopes exist (`packages/tools/src/json_diagnostics.zig:245-311`).

**Success envelope** (emitted when analysis completes with no error-severity diagnostics):

```json
{
  "success": true,
  "proof": {
    "env_vars": ["JWT_SECRET"],
    "outbound_hosts": ["api.stripe.com"],
    "virtual_modules": ["zigttp:auth", "zigttp:cache"],
    "properties": {
      "retry_safe": true,
      "idempotent": true,
      "injection_safe": true,
      "deterministic": false,
      "read_only": false,
      "state_isolated": true,
      "fault_covered": true
    }
  },
  "diagnostics": [ /* warnings only */ ]
}
```

Fields:

- `success` (boolean) — `true` on this envelope.
- `proof` (object) — present when a handler contract was produced. Absent when the tool ran in a mode that does not produce a contract. Clients must not assume `proof` exists.
  - `env_vars` (string array) — environment variables the handler reads literally.
  - `outbound_hosts` (string array) — static egress destinations extracted from the contract.
  - `virtual_modules` (string array) — `zigttp:*` module specifiers the handler imports.
  - `properties` (object, closed set in v1) — seven boolean flags, all keys guaranteed: `retry_safe`, `idempotent`, `injection_safe`, `deterministic`, `read_only`, `state_isolated`, `fault_covered`. New property flags would be a v2 change.
- `diagnostics` (array of `JsonDiagnostic`) — warning-severity diagnostics only. An empty array is normal.

**Error envelope** (emitted when analysis produced one or more error-severity diagnostics):

```json
{
  "success": false,
  "diagnostics": [ /* errors and warnings */ ]
}
```

There is no `proof` key on the error envelope in v1. Clients must branch on `success` before reading proof.

## Virtual-module and feature catalogs

Two ancillary JSON outputs exist and are covered by v1 (`packages/tools/src/json_diagnostics.zig:317-430`):

- `zigts modules --json` emits an array of `{ specifier, exports }` objects, where `exports` is an array of function-name strings.
- `zigts features --json` emits an array of `{ name, status, alternative }` objects, where `status` is `"allowed"` or `"blocked"` and `alternative` is a string or `null`.

These are both additive: new specifiers, exports, features, or statuses can appear, but existing entries cannot change meaning.

## Stability guarantees

For as long as v1 is advertised by `zigts meta` (via `mode: "embedded"` and the `compiler_version` / `policy_version` fields), clients may rely on:

1. Every field and type documented above.
2. Every `ZTS0xx`/`ZTS1xx`/`ZTS2xx`/`ZTS3xx`/`ZVM0xx` code number retaining its meaning.
3. Exit codes as specified per-command.
4. Unknown fields being ignored by all first-party clients — the contract is open for extension.
5. Stable JSON output ordering within a single invocation (across invocations the order is not guaranteed).

Clients may **not** rely on:

1. Human-readable `message` strings being byte-stable.
2. Diagnostic `suggestion` strings being byte-stable.
3. The non-JSON textual outputs of any command.
4. Error messages on stderr.
5. Anything not listed in this document.

## Freezing procedure

This document is v1 as of `2026-04-14`. The file is the source of truth. Any change to `packages/tools/src/expert.zig`, `packages/tools/src/json_diagnostics.zig`, `packages/tools/src/edit_simulate.zig`, `packages/tools/src/review_patch.zig`, `packages/tools/src/describe_rule.zig`, `packages/tools/src/search_rules.zig`, or `packages/tools/src/module_audit.zig` that alters the shape of a documented response must either:

1. Be additive (a new field, a new code, a new category) and leave v1 intact, or
2. Ship as v2, bumping `compiler_version` or an explicit `contract_version` field and updating this document.

Two tripwires guard this contract and run under `zig build test`: the in-tree tests in `packages/tools/src/expert.zig` pin versions and rule-count invariants, and the `test-expert-golden` step asserts byte-identical stdout from the built `zigts` binary against fixtures in `packages/tools/tests/fixtures/expert/`. Regenerate a fixture only after a deliberate contract change and update this document in the same commit.

## What is explicitly out of scope for v1

- `zigts prove`, `zigts mock`, `zigts link`, `zigts rollout`, `zigts compile`: their outputs are either text-only or JSON not yet stabilized. The TUI will consume them through in-process Zig APIs, not through this contract. If a JSON form is later promoted, it becomes v2.
- Streaming output. v1 is request/response only.
- Anything reading from files other than `--stdin-json` on `edit-simulate` and `review-patch`.
- Authentication, rate limiting, tracing, and the managed-task surface for `zigttp serve/dev/deploy`. All runtime and deploy commands are outside v1.

## `zigts expert --print --mode json` event stream

`zigts expert` calls the Anthropic API directly using `ANTHROPIC_API_KEY`. Its system prompt comes entirely from the binary: the shipped persona and bundled references are embedded at compile time via `@embedFile`, and compiler metadata is rendered from the running binary's in-process registries. Startup does not read `AGENTS.md`, `CLAUDE.md`, external skill files, or any other workspace prompt files.

`--print <prompt>` runs one turn and exits. `--mode json` switches output to an NDJSON event stream, one event per line. This surface falls outside the v1 tool contract above but appears here because CI scripts commonly use both together.

**Event schema:** `{"v":1,"k":"<kind>","d":<payload>}`

```json
{"v":1,"k":"user_text","d":"add a GET /health route"}
{"v":1,"k":"tool_use","d":{"id":"tu_01","name":"zigts_check","args_json":"{\"path\":\"handler.ts\"}"}}
{"v":1,"k":"tool_result","d":{"tool_use_id":"tu_01","tool_name":"zigts_check","ok":true,"body":"..."}}
{"v":1,"k":"model_text","d":"Added the route. Proof: retry_safe=true, idempotent=true."}
{"v":1,"k":"end"}
```

Fields:
- `v` (integer) - schema version, currently `1`.
- `k` (string) - event kind. Defined kinds: `user_text`, `model_text`, `tool_use`, `tool_result`, `proof_card`, `diagnostic_box`, `end`.
- `d` (any) - payload. String for `user_text`, `model_text`, `proof_card`, `diagnostic_box`. Object for `tool_use` and `tool_result`. Absent for `end`.

The live stream and `events.jsonl` share the same `{"v","k","d"}` envelope for transcript events, so one parser can handle both if it treats `end` as a live-only sentinel. `end` appears last only on successful one-turn runs; errored runs may terminate without it, and session files neither write nor reconstruct it. Ignore unknown kinds; treat their `d` as a string or object.

This surface is outside the v1 stability guarantee. New kinds may appear in future releases; clients should ignore unknown `k` values.

## Relationship to the TUI port

`zigts expert` uses these commands from in-process function calls. The in-process API must remain a faithful representation of this contract so the non-interactive binary and the expert UI never diverge. The wire shapes above are the reference; the in-process Zig structs are an optimization that must round-trip through them without loss.
