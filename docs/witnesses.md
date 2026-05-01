# Witness Corpus

Every counterexample the compiler discovers when proving a handler is a
concrete falsifying input: a `Request` plus the virtual-module stub
responses needed to drive the handler down the witnessing path. The
witness corpus persists those inputs to disk so they accumulate across
builds, deploys, live reloads, and developer machines.

The corpus is the project's accumulated evidence. It is not throwaway
state.

## Layout

All paths are relative to the project root (the cwd of the analyzer
invocation):

```
.zigttp/witnesses/<short_hash>/handler.path        # text, original handler path
.zigttp/witnesses/<short_hash>/index.jsonl         # append-only event log
.zigttp/witnesses/<short_hash>/<key>.witness.jsonl # one file per witness
.zigttp/witnesses/<short_hash>/<key>.pinned        # marker file (presence = pinned)
```

`<short_hash>` is the first sixteen hex characters of
`sha256(handler_path)`. The original path is recorded in `handler.path`
so the listing surfaces (`zigttp witnesses list`, `pi_witnesses`) can
present it without reversing the hash.

`<key>` is `counterexample.CounterexampleWitness.stableKey()`: a sha256
over `(property, origin_node_id, sink_node_id)`. AST node ids survive
line shifts caused by edits above the witnessing site, so the same
logical leak is not re-persisted as a new entry every time the file is
reformatted.

Witness files use `counterexample.writeJsonl` exactly. A persisted
witness can be replayed via `zigttp mock --replay` against the same
handler without schema translation.

## How the corpus grows

Witnesses are materialised by the agent-facing tools that already
synthesise concrete inputs from a flow_checker diagnostic's constraint
chain:

- `pi_repair_plan` materialises a witness for every flow violation that
  matches a requested goal, then persists each one before returning.
- `pi_goal_check` materialises a witness for every flow violation that
  matches an active goal and persists it during the same pass.

A handler that has never been touched by either tool has no corpus.
Once an agent runs them, the corpus begins populating immediately and
stays in sync with the proof state.

Build-time auto-population is wired into `precompile.runCheckOnly`, so
every `zigts check` and `zig build -Dhandler=...` populates the corpus
from any flow-property witness the analyzer produces. The `proof.witnesses`
block in the JSON envelope reflects the corpus state after that
population pass.

Cause-only specs (`deterministic`, `read_only`, `retry_safe`,
`idempotent`, `state_isolated`, `fault_covered`) do not have flow-style
falsifying inputs - their classification is structural. Seed those
entries explicitly with `zigttp witnesses synthesize <handler> <spec>`.

## Surfaces

### CLI: `zigttp witnesses`

```text
zigttp witnesses list [<handler>]
zigttp witnesses pin <handler> <key|prefix>
zigttp witnesses unpin <handler> <key|prefix>
zigttp witnesses prune <handler> [--older-than <seconds>]
zigttp witnesses synthesize <handler> <spec>
```

`list` with no handler argument summarises every corpus directory found
under `.zigttp/witnesses/`. With a handler, it prints one line per
witness with key prefix, property, pinned status, and the
natural-language summary the analyzer produced.

`pin` and `unpin` accept any unique key prefix. Pinned witnesses are
protected from `prune`.

`prune` removes unpinned witnesses whose first-seen timestamp is older
than the cutoff. The default is 90 days.

`synthesize` seeds a structural witness for one of the cause-only specs.
Repeat invocations on the same `(handler, spec)` pair are idempotent;
the underlying key is `sha256("structural" | spec | handler_path)`. The
stored summary is the per-property `Try:` suggestion from
`spec_discharge.suggestionFor`. Flow-rich specs are rejected because the
analyzer-driven path already covers them.

### JSON envelope

`zigts check --json` includes a `proof.witnesses` block:

```json
{
  "proof": {
    "...": "...",
    "witnesses": {
      "total": 3,
      "by_property": {
        "injection_safe": 1,
        "no_secret_leakage": 2
      }
    }
  }
}
```

The block is read directly from `.zigttp/witnesses/<short_hash>/` for
the handler under check. A missing or empty corpus reports
`{"total":0,"by_property":{}}`.

### Studio: `/_zigttp/studio/witness/<key>.json`

While the studio is running (`zigttp studio <handler.ts>` or
`zigttp serve --studio --watch --prove`), each row in the Witnesses
tile is clickable. Clicking fetches the on-disk witness file and
renders the falsifying request, IO stubs, summary, source location,
and pinned status inline. The endpoint is read-only:

```text
GET /_zigttp/studio/witness/<key>.json
=> {"key":"<hex>", "pinned":<bool>, "events":[<witness>, <request>, <io>...]}
```

`<key>` must be 1..64 hex chars (the same shape `witness_corpus.persist`
produces). The events array is the on-disk JSONL re-emitted as a JSON
array; each entry is an unmodified record from
`counterexample.writeJsonl`. Studio caches nothing. It re-reads the
file on every click, so a corpus refresh on disk is visible
immediately. A fingerprint guard on the detail panel keeps an
already-expanded witness expanded across the 750ms poll cycle when the
underlying `(key, property, pinned)` set is unchanged.

### Agent tool: `pi_witnesses`

Single op: given a handler path, return the corpus listing.

```json
{
  "ok": true,
  "handler_path": "examples/system/users.ts",
  "total": 3,
  "by_property": { "injection_safe": 1, "no_secret_leakage": 2 },
  "entries": [
    {
      "key": "abcdef0123456789...",
      "property": "no_secret_leakage",
      "summary": "SECRET_KEY flows into Response body",
      "pinned": false,
      "first_seen_unix_s": 1745539200
    }
  ]
}
```

Use `pi_witnesses` before drafting a repair against a `Spec` failure.
`Spec`s with zero witnesses are unprobed: the proof currently relies on
the classifier alone, and a regression there would silently slip past
the corpus. `Spec`s with pinned witnesses are load-bearing; a repair
that removes them weakens coverage.

The slash command `/witnesses <path>` invokes the same tool from the
expert REPL.

## Versioning

The corpus is per-project state. Commit `.zigttp/witnesses/` to your
repository so every contributor's build sees the same defending
evidence and CI fails on the same regressions. Pinned entries should
in particular be treated as part of the source of truth.

## See also

- [verification.md - Author-Declared Spec Discharge](verification.md#8-author-declared-spec-discharge)
- `packages/zigts/src/witness_corpus.zig` - persistence library
- `packages/zigts/src/counterexample.zig` - solver and JSONL wire format
- `packages/runtime/src/witnesses_cli.zig` - `zigttp witnesses` subcommand
- `packages/pi/src/tools/pi_witnesses.zig` - agent tool
