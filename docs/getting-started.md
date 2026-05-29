# Getting Started

This guide takes a new zigttp project from an empty directory to a verified
local deploy using the five commands you need: `init`, `dev`, `test`, `expert`,
and `deploy`.

## The Five Commands

Everything in day-to-day use is one of five verbs:

- `zigttp init` - create a project.
- `zigttp dev` - run it locally; watch and prove on every save.
- `zigttp test` - run the handler test fixture.
- `zigttp expert` - the interactive compiler-in-the-loop agent.
- `zigttp deploy` - build, prove, and deploy.

Advanced commands (the proof ledger, project diagnostics, the analyzer,
cloud capability grants, and more) stay out of the way until you need them.
Run `zigttp help --all` to see them; each keeps its own `--help`.

## Proof Passport Demo

For a local, noninteractive walkthrough of the proof model:

```bash
zigttp demo --scripted --out proof-demo --export proof-demo/passport
```

Open `proof-demo/passport/index.html` to inspect the exported Proof Passport.
It captures the baseline proof, an unsafe edit with a secret-flow witness, the
repair, and the local deploy receipt.

## 1. Install

Pre-built binaries for macOS and Linux (x86_64, aarch64):

```bash
curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
zigttp --help
```

This installs the `zigttp` command. That is the only binary you need to follow
this guide.

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
zigttp dev
```

`dev` checks the project shape and configured entry, runs the analyzer, starts
the server, watches the handler and local imports, and proves each save before
swapping the running handler. A failing check prints the proof card and the
failing stage before the server starts, so there is no separate readiness or
verify step to run first.

Try the API starter:

```bash
curl http://127.0.0.1:3000/health
```

## 4. Test

Run the project fixture:

```bash
zigttp test
```

`zigttp test` runs the analyzer first, then defaults to
`tests/handler.test.jsonl`. Pass a path when you want to run a different
fixture.

## 5. The Expert

`zigttp expert` opens an interactive coding agent that runs the same analyzers
the compiler uses. It can explain a diagnostic, verify an edit, and propose a
fix against your handler as you work.

Store an Anthropic API key once, then launch the agent:

```bash
zigttp auth claude     # paste a key from console.anthropic.com (input hidden)
zigttp expert
```

`auth claude` writes the key to `~/.zigttp/providers.json` at mode 0600 and the
runtime auto-injects it on launch. A shell-exported `ANTHROPIC_API_KEY` works
too and takes precedence. For a guided walkthrough, see
[`examples/hello-claude/`](../examples/hello-claude/README.md).

## 6. Deploy

```bash
zigttp deploy
./.zigttp/deploy/my-app -p 3001
curl http://127.0.0.1:3001/health
```

Bare `deploy` is local. It verifies the handler, emits a self-contained binary
using the `zigttp-runtime` template at `.zigttp/deploy/<project-name>`, and
appends a `kind=deploy` row to `.zigttp/proofs.jsonl`. The command output
points at `zigttp proofs list` if you want to inspect the ledger.

## 7. Try The HTMX Starter

The HTMX starter uses a TSX handler entry:

```bash
cd ..
zigttp init htmx-app --template htmx
cd htmx-app
test -f src/handler.tsx
zigttp test
```

The generated `zigttp.json` points at `src/handler.tsx`, so `test`, `dev`, and
`deploy` all use the TSX stripping path.

## Common Next Steps

- Read [Reading the Proof Card](proof-card.md) to understand the verdict that
  `dev` and `test` print on every save.
- Add `zigttp:env` for configuration values.
- Add `zigttp:decode` and `zigttp:validate` for request payloads.
- Add `zigttp:sql` with `--sqlite <file>` for SQLite-backed handlers.
- Add `zigttp:fetch` with `--outbound-http` or `--outbound-host <host>` for
  outbound HTTP.
