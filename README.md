<p align="center">
  <img src="docs/zigttp-logo.jpg" alt="zigttp" width="600">
  <p align="center"><a href="https://zigttp.timok.deno.net/">Website</a> - <a href="docs/README.md">Documentation</a></p>
</p>

# Compile-time proof you can see

zigttp is a pure-Zig JavaScript engine plus a serverless runtime. One
binary, no dependencies, instant cold starts. The compiler is the
whole product: every JavaScript restriction (no `var`, no `while`, no
`class`, no `try/catch`) buys a specific compile-time proof. Your
handler code is the spec.

**Edit your handler. Watch the proof flip live.** Every save is
recompiled and proven. The terminal proof card shows your handler's
declared `Spec<...>` obligations - `deterministic`,
`no_secret_leakage`, `injection_safe`, `idempotent` - discharged in
real time. Drop a `Date.now()` into the body and `-deterministic`
lights up red with a `Why:` row pointing at the source line. Wrap it
in `step("ts", () => Date.now())` from `zigttp:durable` and the chip
flips back green.

```bash
zigttp init my-app && cd my-app && zigttp dev
```

See [`zigts restrictions`](docs/restrictions-to-proofs.md) for the
cut-to-proof map and [`examples/README.md`](examples/README.md) for
the curated reading order.

## Install

Pre-built binaries for macOS and Linux (x86_64, aarch64):

```bash
curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
```

Or download a tarball from [GitHub Releases](https://github.com/srdjan/zigttp/releases).

To build from source (requires Zig `0.16.0`):

```bash
git clone https://github.com/srdjan/zigttp.git && cd zigttp
zig build -Doptimize=ReleaseFast
```

## Quick Start

Day-to-day use is five commands: `init`, `dev`, `test`, `expert`,
`deploy`. Each auto-detects the project from `zigttp.json`, so most
steps take no arguments. Everything else stays in the background until
you need it; run `zigttp help --all` for the full list.

```bash
zigttp init my-app && cd my-app   # scaffold a new project
zigttp dev                        # proof-aware live reload
# ... edit src/handler.ts (HTMX projects use src/handler.tsx)
zigttp test                       # run the starter fixture
zigttp deploy                     # self-contained signed binary
./.zigttp/deploy/my-app
```

For the full first project path, see
[Getting Started](docs/getting-started.md). For the verdict that `dev`
and `test` print on every save, see
[Reading the Proof Card](docs/proof-card.md).

## Handler Example

```jsx
function HomePage() {
    return (
        <html>
            <head><title>Hello World</title></head>
            <body><h1>Hello World</h1></body>
        </html>
    );
}

function handler(request) {
    if (request.url === "/") {
        return Response.html(renderToString(<HomePage />));
    }
    if (request.url === "/api/echo") {
        return Response.json({ method: request.method, url: request.url });
    }
    return Response.text("Not Found", { status: 404 });
}
```

See [examples/](examples/) for HTMX, SQL, auth, durable workflows,
and more.

## What makes it different

- **Opinionated language subset.** TypeScript with the footguns
  removed. Each restriction buys a specific compiler guarantee.
  [Language subset](docs/language-subset.md) - [Restrictions to
  proofs](docs/restrictions-to-proofs.md).
- **Compile-time verification.** Every code path returns a Response,
  Result values are checked before access, no unreachable code, no
  cross-request state leakage. [Verification](docs/verification.md).
- **Sound mode.** Type-directed analysis catches non-numeric
  arithmetic, mixed-type `+`, and tautological comparisons at compile
  time. [Sound mode](docs/sound-mode.md).
- **Automatic runtime sandboxing.** The compiler extracts a contract
  and derives a least-privilege runtime policy from it. No config
  required. [Contracts and sandboxing](docs/contracts-and-sandboxing.md).
- **Author-declared specs.** Declare which proven properties your
  handler must satisfy directly in the return type:
  `Response & Spec<"idempotent" | "deterministic">`. [Spec
  reference](docs/contracts-and-sandboxing.md#author-declared-specs-spec).
- **Proof receipts (default-on).** Every `zigttp deploy` signs a JWS
  over `(bytecode, contract, policy)` with a persistent Ed25519
  identity. `zigttp verify <url>` validates from any machine.
  [Attestation](docs/roadmap/attest-slice-2.md).
- **Deterministic replay and proven evolution.** Record I/O with
  `--trace`, replay with `--replay`, compare contracts across
  versions with `-Dprove`. [Replay and
  evolution](docs/contracts-and-sandboxing.md#deterministic-replay---trace---replay--dreplay).
- **Durable execution.** `run(key, fn)`, `step(name, fn)`,
  `sleep(ms)`, `waitSignal(name)` from `zigttp:durable` with
  write-ahead oplog and crash recovery.
  [zigttp:durable](docs/virtual-modules/workflow/durable.md).
- **Structured concurrent I/O.** `parallel()` and `race()` from
  `zigttp:io` overlap outbound HTTP without async/await or Promises.
  [zigttp:io](docs/virtual-modules/workflow/io.md).
- **Native virtual modules.** JWT, JSON Schema, cache, SQL, fetch,
  WebSocket, durable, scope - implemented in Zig with capability
  declarations enforced at call time.
  [Virtual modules index](docs/virtual-modules/README.md).
- **`zigts expert` interactive agent.** Compiler-in-the-loop coding
  agent with a property-goal autoloop where the compiler (not the
  LLM) drives convergence. [CLI reference](docs/cli.md#zigts-expert-interactive-agent),
  [autoloop](docs/roadmap/autoloop.md).

## Numbers

~3.5ms cold-start floor, ~7-15ms typical cold start. 4.8MB deployed
binary. ~13MB memory baseline. Pre-warmed handler pool with
per-request isolation. See [performance docs](docs/performance.md)
for the measured cold-start distribution and the Node/Deno throughput
comparison.

## Documentation

Start at the [Documentation Index](docs/README.md). High-traffic
pages:

- [User Guide](docs/user-guide.md) - handler API, routing, JSON,
  tests, troubleshooting
- [CLI Reference](docs/cli.md) - every `zigttp` and `zigts`
  subcommand, the expert REPL, edge, proofs, verify, studio
- [Contracts and Auto-Sandboxing](docs/contracts-and-sandboxing.md) -
  contract extraction, policy, OpenAPI/SDK emit, replay, evolution,
  `Spec<...>`
- [Virtual Modules](docs/virtual-modules/README.md) - canonical module
  index and per-module API pages
- [Verification](docs/verification.md), [Sound
  Mode](docs/sound-mode.md), [TypeScript](docs/typescript.md), [JSX
  Guide](docs/jsx-guide.md)
- [Performance](docs/performance.md), [Reliability and
  Limits](docs/reliability.md)
- [Architecture](docs/internals/architecture.md)

## Roadmap

zigttp is at v0.1.0-beta. Direction is set by two documents kept
current in the repo:

- [v1 Public Release](docs/roadmap/v1-public-release.md) - release
  contract and gates for the first public release.
- [Frontier](docs/roadmap/frontier.md) - strategic thesis: a
  proof-aware TypeScript execution platform where the execution model
  is compiler-visible, effects are explicit imports, and contracts
  are first-class artifacts.

Further notes (attestation slices, autoloop, proofable third-party
modules, hosted proof dashboard) live in
[docs/roadmap/](docs/roadmap/).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for build, test, and the
conventions a PR must respect. Security disclosure policy:
[SECURITY.md](SECURITY.md).

## License

MIT.

## Credits

- **zigts** - pure Zig JavaScript engine (part of this project)
- [Zig](https://ziglang.org/) programming language
- Codex and Claude
