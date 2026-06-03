<p align="center">
  <img src="docs/zigttp-logo.jpg" alt="zigttp" width="600">
  <p align="center"><a href="https://zigttp.timok.deno.net/">Website</a> - <a href="docs/README.md">Documentation</a></p>
</p>

# zigttp

zigttp is a pure-Zig JavaScript and TypeScript runtime for HTTP handlers.
It ships as one binary, runs without npm or Node, and uses a restricted
language profile so the compiler can prove useful handler properties before
the server starts.

The daily workflow is small:

```bash
zigttp init my-app && cd my-app
zigttp dev
zigttp test
zigttp deploy
```

`zigttp dev` watches the handler, recompiles it, and prints a proof card on
every save. `zigttp deploy` builds a self-contained local binary, writes a
proof ledger entry, and signs a proof receipt by default.

## Install

Pre-built binaries are published for macOS and Linux on x86_64 and aarch64:

```bash
curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
```

Or build from source with Zig `0.16.0`:

```bash
git clone https://github.com/srdjan/zigttp.git
cd zigttp
zig build -Doptimize=ReleaseFast
```

## First Handler

```tsx
function HomePage() {
    return (
        <html>
            <head><title>Hello</title></head>
            <body><h1>Hello from zigttp</h1></body>
        </html>
    );
}

function handler(req) {
    if (req.path === "/") {
        return Response.html(renderToString(<HomePage />));
    }
    if (req.path === "/api/echo") {
        return Response.json({ method: req.method, path: req.path });
    }
    return Response.text("Not Found", { status: 404 });
}
```

See [examples/](examples/) for routing, JSX/TSX, SQL, fetch, durable
workflows, WebSocket, and proof examples.

## Current Surface

- Five core commands: `init`, `dev`, `test`, `expert`, `deploy`. Advanced
  commands are listed by `zigttp help --all`.
- Handler API: `function handler(req): Response`, plus `Response.text`,
  `Response.json`, and `Response.html`.
- Language profile: a restricted JS/TS/TSX subset with no `var`, `while`,
  `class`, or `try/catch`; unsupported constructs fail at compile time.
- Proofs: response-path verification, Result/optional checks, state-isolation
  checks, active `Spec<...>` obligations, flow checks, proof traces, witnesses,
  and proof receipts.
- Virtual modules: 22 native modules under `zigttp:*` for env, crypto, auth,
  validation, cache, SQL, fetch, service calls, WebSocket, routing, durable
  workflows, structured I/O, logging, IDs, time, text, and more.
- Local deploy: self-contained binary output under
  `.zigttp/deploy/<project-name>` with default-on attestation.

## Numbers

Benchmark claims are kept in [Performance](docs/performance.md). The measured
baseline is roughly a 3.5 ms cold-start floor, 7-15 ms typical cold start
depending on host load, about 13 MB RSS after first response, and about 112k
req/s on the documented HTTP benchmark.

## Documentation

Start at the [Documentation Index](docs/README.md).

- [User Guide](docs/user-guide.md) - setup, handlers, routing, testing,
  deployment, proof receipts, and troubleshooting.
- [CLI Reference](docs/cli.md) - core commands and advanced machine tools.
- [Virtual Modules](docs/virtual-modules/README.md) - complete current module
  list and runtime requirements.
- [Contracts and Sandboxing](docs/contracts-and-sandboxing.md) - contract
  extraction, runtime policy, replay, OpenAPI, SDK emit, and `Spec<...>`.
- [Verification](docs/verification.md), [TypeScript](docs/typescript.md),
  [Sound Mode](docs/sound-mode.md), and
  [Restrictions to Proofs](docs/restrictions-to-proofs.md).
- [Performance](docs/performance.md), [Reliability](docs/reliability.md),
  [Roadmap](docs/roadmap.md), and
  [Architecture](docs/internals/architecture.md).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). Security reports go through
[SECURITY.md](SECURITY.md).

## License

MIT.
