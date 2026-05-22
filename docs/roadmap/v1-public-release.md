# v1 Public Release Roadmap

This roadmap defines the first public Zigttp release around one showcase flow:

```bash
zigttp init my-app
cd my-app
zigttp dev
# edit src/handler.ts
zigttp check
zigttp build
zigttp deploy
zigttp proofs badge
```

`zigttp deploy` is local by default for v1. It emits `.zigttp/deploy/<app>`
and appends a proof-ledger row without credentials, Docker, or network access.
Hosted deploy is deferred from v0.1.0-beta: the `--cloud` path and the account
commands are gated at the CLI boundary (see Milestone 5).

## Release Contract

- `zigttp init <name>` creates a safe project folder and refuses path-like or
  shell-confusing names.
- `zigttp dev` starts proof-aware live reload for the project entry in
  `zigttp.json`.
- Handler authoring happens in the user's editor while the terminal HUD watches
  and re-verifies on save.
- `zigttp studio` remains the browser proof workbench for the same live state.
- `zigttp check` is the fast terminal verifier.
- `zigttp build` verifies the handler and emits `.zigttp/build/<app>`.
- `zigttp deploy` verifies the handler, emits `.zigttp/deploy/<app>`, and
  records `kind=deploy` in `.zigttp/proofs.jsonl`.
- `zigttp deploy --cloud` and the hosted account commands are deferred from
  v0.1.0-beta (see Milestone 5).

## Milestone 1: Project Scaffolding

Status: in progress.

Required:

- Scaffold `zigttp.json`, `src/handler.ts`, `tests/handler.test.jsonl`,
  `public/`, `.gitignore`, and `README.md`.
- Reject unsafe project names before writing files.
- Keep starter handlers small, readable, and verifiable.
- Print next steps that match the public release flow, including `dev` and the
  proof badge artifact.

Acceptance:

```bash
zigttp init smoke-app
cd smoke-app
zigttp doctor
zigttp check
```

## Milestone 2: Studio Workbench

Status: existing proof dashboard, needs release polish.

Required:

- Serve `/_zigttp/studio` from `zigttp studio`.
- Show loading, ready, and error states.
- Show verdict, proven properties, routes, env, egress, capabilities,
  declared specs, witnesses, and generated tests.
- Re-run analysis on handler changes.
- Keep Studio bound to `127.0.0.1` unless the user explicitly changes host.

Acceptance:

```bash
zigttp studio
curl -sf http://127.0.0.1:3000/_zigttp/studio/state.json
```

The state endpoint must eventually return `status: "ready"` with a verdict for
the scaffolded handler.

## Milestone 3: Verify And Build

Status: existing, needs negative-path coverage.

Required:

- `zigttp check` auto-detects the project entry when no handler path is given.
- `zigttp build` refuses to emit a binary on parse, type, or proof failure.
- Build output defaults to `.zigttp/build/<app>`.
- Failure diagnostics distinguish missing project, missing handler, invalid
  source, and verification failure.

Acceptance:

```bash
zigttp check
zigttp build
./.zigttp/build/<app> -p 3001
curl -sf http://127.0.0.1:3001/
```

## Milestone 4: Local Deploy

Status: existing, now the documented default path.

Required:

- `zigttp deploy` and `zigttp deploy --local` both target local deploy.
- Local deploy never requires credentials, Docker, registry access, or network.
- The deployed binary is executable and self-contained.
- `.zigttp/proofs.jsonl` gets a `kind=deploy` row only after verification and
  artifact creation succeed.

Acceptance:

```bash
zigttp deploy
./.zigttp/deploy/<app> -p 3002
curl -sf http://127.0.0.1:3002/
zigttp proofs show HEAD
```

## Milestone 5: Hosted Deploy Preview

Status: deferred from v0.1.0-beta.

The hosted control-plane deploy is feature-complete but lacks integration-test
coverage, so it is hidden for the beta. `zigttp deploy --cloud`, `login`,
`logout`, `review`, `grants`, and `revoke-grant` are gated at the CLI boundary
and reject with a "not in this beta" message; the control-plane code stays in
`packages/runtime/src/deploy/`. Re-enabling is a matter of restoring the CLI
dispatch once the path has CI smoke coverage.

Required before re-enabling:

- A cloud smoke test that exercises the path end-to-end against the control
  plane, run separately from the local release smoke (credentials and network).
- Docs that mark hosted deploy as preview and avoid showing it as the default
  release path.

## Milestone 6: Release Gates

Status: existing smoke gate with negative-path coverage, needs to stay
required.

Required:

- Keep `zig build smoke-v1` wired to `scripts/smoke-v1.sh`.
- Exercise the exact public path: init, doctor, check, dev/studio, build, run,
  deploy, run, badge.
- Keep negative-path smoke cases for invalid project names, missing project
  diagnostics, and broken handler build refusal.

Required checks before release:

```bash
zig fmt --check build.zig packages/runtime/src/dev_cli.zig
zig build test
zig build smoke-v1
```

## Open Decisions

- Whether Studio should remain an observer-only workbench for v1 or include a
  small browser editor. The current release contract assumes editor-based
  authoring.
- Whether cloud-only flags should continue implying `--cloud` for compatibility
  or require explicit `--cloud`. Deferred along with hosted cloud deploy;
  revisit when the path is re-enabled.
