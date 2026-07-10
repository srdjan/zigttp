<p align="center">
  <img src="docs/zigttp-logo.jpg" alt="zigttp" width="600">
  <p align="center"><a href="https://zigttp.timok.deno.net/">Website</a> - <a href="docs/README.md">Documentation</a></p>
</p>

# zigttp

**The AI writes your handler. The compiler proves it's safe.**

zigttp is a pure-Zig JavaScript/TypeScript runtime for HTTP handlers. Describe
the handler you want in plain English: the built-in agent writes it, and the
compiler proves it before it ships - every path returns a Response, no secret
leaks, declared egress only. zigttp ships as one binary, runs without npm or
Node, and uses a restricted language profile so those proofs are cheap and
automatic.

```bash
zigttp init my-app --expert
# then: "add a GET /health route and a POST /echo that validates JSON"
```

The agent proposes a compiler-verified edit and you approve only code that
passes. When your intent is ambiguous, it asks one question instead of guessing.

Prefer to write the handler yourself? The same compiler runs the watch loop:

```bash
zigttp dev      # recompile and print a proof card on every save
zigttp test
zigttp deploy   # self-contained binary, proof ledger entry, signed receipt
```

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
import type { Spec } from "zigttp:types";

type Guardrails = Spec<
    | "deterministic"
    | "no_secret_leakage"
    | "injection_safe"
>;

function HomePage(): JSX.Element {
    return (
        <html>
            <head><title>Hello</title></head>
            <body><h1>Hello from zigttp</h1></body>
        </html>
    );
}

function handler(req: Request): Response & Guardrails {
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
  `Response.json`, and `Response.html`, and `resource(data, affordances)` for
  content-negotiated HAL-JSON and HTMX from one declaration.
- Language profile: a restricted JS/TS/TSX subset with no `var`, `while`,
  `class`, or `try/catch`; unsupported constructs fail at compile time.
- Proofs: response-path verification, Result/optional checks, state-isolation
  checks, active `Spec<...>` obligations, flow checks, proof traces, witnesses,
  and proof receipts. A runtime fault that slips through names the proof chip
  that guards it and the faulting source line, instead of a bare 500.
- Virtual modules: native modules under `zigttp:*` for env, crypto, auth,
  validation, cache, SQL, fetch, service calls, WebSocket, routing, durable and
  multi-handler workflows, structured I/O, logging, IDs, time, text, and more.
- Local deploy: self-contained binary output under
  `.zigttp/deploy/<project-name>` with default-on attestation.

## Security model

Read the [Threat Model](docs/threat-model.md) before running untrusted code or
exposing a binary publicly. Two boundaries are easy to miss:

- `dev` and `serve` from source are not a sandbox. They run handler code with
  your user's permissions for fast iteration. The enforced surfaces are the
  precompiled (`-Dhandler=`) and `deploy` binaries, which carry and enforce the
  contract-derived capability allowlist (egress, env, cache, SQL).
- No TLS. The runtime serves plain HTTP and binds `127.0.0.1` by default.
  Terminate TLS at a reverse proxy and set the host explicitly before exposing a
  deployed binary to public traffic.
- `expert` sends your handler source to the configured model provider
  (Anthropic or OpenAI). Attestation is on by default and publishes a stable
  per-user public-key fingerprint at `/.well-known/zigttp-attest`.

## Numbers

Benchmark claims are kept in [Performance](docs/performance.md). The measured
baseline is roughly a 3.5 ms cold-start floor, 7-15 ms typical cold start
depending on host load, about 13 MB RSS after first response, and about 112k
req/s on the documented HTTP benchmark. These numbers are pending receipt-backed
measurement in this repo; `zig build bench` covers the in-process benchmark
suite, while cold-start/RSS/HTTP-throughput evidence still comes from the
external/manual harness noted in the performance docs.

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
