# Proof Gate

`zigttp proofs gate` carries the proof to where review happens: the pull
request. The [Proof Passport demo](user-guide.md#proof-passport-demo)
shows the proof story for one scripted edit; the gate runs the same analysis
across every handler a real branch changed and turns the result into a
merge-decision signal.

## The verdict is the answer to "is this safe to merge?"

For each changed handler the gate compiles the base and head versions and runs
the same `contract_diff` the expert loop and `zigttp prove-behavior` use, then
aggregates one repo-level verdict:

| Verdict | Meaning | Merge signal |
|---|---|---|
| `equivalent` | Surfaces and every response path are identical. | Mechanically safe. A proven no-op. |
| `equivalent_modulo_laws` | Identical after the algebraic laws declared on virtual modules. | Safe. The report lists which laws the proof used. |
| `additive` | New routes/capabilities, nothing removed or changed. | Backward-compatible. |
| `breaking` | A route/capability was removed, or a response on an existing path changed. | Needs review. The gate fails the check. |

The repo verdict is the worst single handler: one `breaking` handler makes the
whole pull request `breaking`.

## Local use

```bash
# Compare the working tree against the default base (origin/main, then main).
zigttp proofs gate

# Pick the range explicitly, and emit the machine verdict for scripting.
zigttp proofs gate --base origin/main --head HEAD --format json
```

Exit code: `0` safe (equivalent / additive), `1` breaking, `2` usage or git
error. The Markdown form is a ready-to-paste PR comment; the JSON form
(`zigttp.proof-gate.v1`) carries the per-handler verdict, behavior delta,
surface delta, and counterexamples.

Flags:

- `--base <ref>` before side. Default `origin/main`, falling back to `main`.
- `--head <ref>` after side. Default: the working tree.
- `--format md|json` default `md`.
- `--out <path>` write the report to a file instead of stdout.
- `--no-sign` skip the signed `kind=equivalence` ledger rows. Run locally with
  signing on (the default) to append a signed receipt to `.zigttp/proofs.jsonl`;
  CI passes `--no-sign` because runners hold no persistent attest identity.

Files that are not handlers (a changed `.ts`/`.js` with no routes and no
behavior paths, i.e. a config or library module) and files under `tests/`,
`fixtures/`, or matching `*.test.*`/`*.spec.*` are skipped, and listed as such
in the report.

## GitHub Actions

The shipped workflow posts a sticky proof comment on every pull request and
fails the check on `breaking`. Copy `.github/workflows/proof-gate.yml` and
`scripts/proof-gate.sh` into your project.

```yaml
on: pull_request
permissions:
  contents: read
  pull-requests: write
# checkout with fetch-depth: 0 so `git show <base>:<path>` can read the
# before side, build zigttp, run scripts/proof-gate.sh, post the comment,
# then exit 1 when the verdict is `breaking`.
```

To merge a deliberate breaking change, add the `proof-override` label to the
pull request; the gate still posts the comment but does not fail the check.

## Relationship to other proof surfaces

- `zigttp dev --prove` and the terminal HUD show the verdict to the author on
  save. The gate shows it to a reviewer at merge time.
- `zigttp proofs show` / `diff` re-render a single ledger entry. The gate
  aggregates across a git range.
- `zigttp prove-behavior <before.ts> <after.ts>` is the one-shot, two-file form
  of the same per-handler verdict the gate computes for each changed file.
