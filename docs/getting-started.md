# Getting Started

This guide takes a new zigttp project from an empty directory to a verified
local deploy. The hosted deploy path is optional and marked preview.

The local path below is covered by:

```bash
zig build smoke-getting-started
```

## 1. Install

Build from source with Zig 0.16.0:

```bash
git clone https://github.com/srdjan/zigttp
cd zigttp
zig build -Doptimize=ReleaseFast
./zig-out/bin/zigttp --help
```

The release build installs three binaries under `zig-out/bin`: `zigttp`,
`zigttp-runtime`, and `zigts`.

## 2. Create A Project

```bash
zigttp init my-app --template api
cd my-app
```

The scaffold writes:

- `zigttp.json` - project entry, host, port, and runtime options.
- `src/handler.ts` - starter handler for `basic` and `api`.
- `src/handler.tsx` - starter handler for `htmx`.
- `tests/handler.test.jsonl` - declarative request fixture.
- `public/` - static file directory placeholder.
- `README.md` - project-local command notes.

Use `--template basic` for the proof tour starter, `--template api` for JSON
endpoints, and `--template htmx` for a TSX/HTMX page with a `.tsx` entry.

## 3. Run Locally

```bash
zigttp doctor
zigttp dev
```

`doctor` checks the project shape, configured entry file, runtime tools, and
analyzer path. `dev` runs the same preflight, starts the server, watches the
handler and local imports, and proves each save before swapping the running
handler. Check failures show the failing stage before the server starts.

Try the API starter:

```bash
curl http://127.0.0.1:3000/health
```

## 4. Check And Test

Run the analyzer once:

```bash
zigttp check
```

Run the project fixture:

```bash
zigttp test
```

`zigttp test` runs the analyzer first, then defaults to
`tests/handler.test.jsonl`. Pass a path when you want to run a different
fixture.

## 5. Build

```bash
zigttp build
./.zigttp/build/my-app -p 3001
curl http://127.0.0.1:3001/health
```

`build` verifies the handler and emits a self-contained binary using the
`zigttp-runtime` template.

## 6. Deploy Locally

```bash
zigttp deploy
./.zigttp/deploy/my-app -p 3002
curl http://127.0.0.1:3002/health
```

Bare `deploy` is local. It writes `.zigttp/deploy/<project-name>` and appends a
`kind=deploy` row to `.zigttp/proofs.jsonl`.

Inspect the proof ledger:

```bash
zigttp proofs list
zigttp proofs show HEAD
zigttp proofs badge
```

`zigttp proofs badge` writes `zigttp-proof.svg` in the project directory.

## 7. Try The HTMX Starter

The HTMX starter uses a TSX handler entry:

```bash
cd ..
zigttp init htmx-app --template htmx
cd htmx-app
test -f src/handler.tsx
zigttp check
zigttp test
```

The generated `zigttp.json` points at `src/handler.tsx`, so `check`, `test`,
`build`, and `dev` all use the TSX stripping path.

## 8. Optional Hosted Deploy

The hosted control-plane path is explicit:

```bash
zigttp deploy --cloud
```

If credentials are missing, the CLI prompts for a Zigttp access token or falls
back to device login. See [Deploy](deploy-tutorial.md) for regions, readiness
waiting, drift checks, and review approvals.

## Common Next Steps

- Read [Reading the Proof Card](proof-card.md) to understand the verdict that
  `check` and `dev` print on every save.
- Add `zigttp:env` for configuration values.
- Add `zigttp:decode` and `zigttp:validate` for request payloads.
- Add `zigttp:sql` with `--sqlite <file>` for SQLite-backed handlers.
- Add `zigttp:fetch` with `--outbound-http` or `--outbound-host <host>` for
  outbound HTTP.
