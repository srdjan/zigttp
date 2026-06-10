# Architecture

zigttp is a Zig monorepo with three runtime-facing binaries:

- `zigttp`: developer CLI, local server, proof tools, expert mode, deploy.
- `zigttp-runtime`: minimal runtime template used by self-contained deploy
  artifacts.
- `zigts`: analyzer/compiler CLI for IDEs, CI, and machine integrations.

The deployed runtime does not link the expert agent. The analyzer and runtime
share the same `zigts` engine and contract logic.

## Packages

| Path | Role |
|---|---|
| `packages/runtime/` | HTTP server, runtime adapter, CLI, local deploy, proof ledger, Studio, edge runtime. |
| `packages/zigts/` | Parser, type checker, verifier, bytecode, interpreter, JIT, contracts, virtual-module registry. |
| `packages/tools/` | Precompile pipeline and analyzer command registry shared by `zigttp` and `zigts`. |
| `packages/modules/` | SDK-pure virtual modules and module spec JSON. |
| `packages/zigttp-sdk/` | Extension SDK types and helpers for native modules. |
| `packages/proof-review/` | Proof-review verdict types and rendering helpers. |
| `packages/pi/` | Compiler-in-the-loop expert agent linked into `zigttp` only. |

`build.zig` wires the packages, build options, tests, smoke checks, and release
steps.

## Request Flow

1. The server parses HTTP input, enforces request limits, and builds a request
   object.
2. A `HandlerPool` slot provides an isolated `ZRuntime`.
3. The runtime compiles or reuses handler bytecode, then invokes
   `function handler(req)`.
4. The handler returns a `Response`.
5. The server validates response headers, writes the response, and releases the
   runtime slot.

Each request runs with isolated runtime state. Request isolation is
arena-based: request-scoped allocations are bulk-reset between requests. The
engine includes a garbage collector, but the default serving configuration
uses the hybrid arena allocator, which disables collection on the serving path
(`Context.setHybridAllocator` in `packages/zigts/src/context.zig`).

## Compiler Pipeline

For a handler source file:

1. Strip supported TypeScript syntax.
2. Parse the restricted JS/TS/TSX grammar.
3. Resolve imports, including `zigttp:*` virtual modules.
4. Run type, path, Result/optional, state-isolation, flow, and spec checks.
5. Extract a handler contract and module capability surface.
6. Emit bytecode and optional artifacts such as contract JSON, OpenAPI, SDK,
   generated tests, or build reports.

Unsupported language features fail before runtime. See
[Feature Detection](../feature-detection.md) and
[Verification](../verification.md).

## Virtual Modules

The native module registry is the source of truth:

- `packages/zigts/src/builtin_modules.zig` lists all built-ins and governance
  entries.
- `packages/modules/module-specs/` stores public module specs.
- `packages/modules/src/root.zig` exposes SDK-pure module bindings.
- Engine-coupled modules stay under `packages/zigts/src/modules/`.

Every export carries effect and capability metadata used by contract
extraction, runtime sandboxing, and handler property classification. The
current module list is in [Virtual Modules](../virtual-modules/README.md).

## Runtime Policy

Precompiled handlers can carry a contract and derived policy. At startup and
per request, the runtime uses that policy to gate native capabilities such as
env access, cache, SQL, outbound HTTP, filesystem-backed state, WebSocket, and
runtime callbacks.

The same contract feeds:

- proof card rendering;
- proof ledger entries;
- `zigttp proofs gate`;
- upgrade checks;
- OpenAPI and SDK output;
- runtime sandbox policy.

## Live Reload

`zigttp dev` watches handler and config files. On a valid save it recompiles
the handler. With `--prove`, the new contract is diffed against the previous
contract before the handler pool is swapped. Compilation failures keep the
currently serving handler active. Accepted swaps rederive the runtime
capability policy and handler-pool lifecycle policy from the new contract
before new runtime generations are acquired.

## Deploy Artifact

`zigttp deploy` builds a self-contained local binary under
`.zigttp/deploy/<project-name>`. The output starts with the `zigttp-runtime`
template and appends a payload containing bytecode, contract JSON, runtime
policy, and optional JWS attestation. `self_extract.zig` validates the trailer,
loads the payload, and starts the runtime.

Attestation signs bytecode, contract, and policy hashes. The running server
emits proof headers and serves `/.well-known/zigttp-attest`; `zigttp verify
<url>` validates the receipt.

## Testing And Governance

Important gates:

```bash
zig build test
zig build test-zigts
zig build test-zruntime
zig build test-module-governance
zig build test-capability-audit
zig build test-docs-drift test-doc-links
bash scripts/test-examples.sh
```

Module governance checks keep the registry, specs, and docs from drifting.
