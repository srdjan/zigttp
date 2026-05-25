# Security Policy

## Supported Versions

Only the latest tagged release and `main` receive security fixes. Release candidates (`-rcN`) are supported until the corresponding final release ships.

## Reporting a Vulnerability

Report suspected vulnerabilities privately via GitHub Security Advisories:

https://github.com/srdjan/zigttp/security/advisories/new

Do not open public issues for security reports. After triage you will receive a remediation plan or a rationale for closing the report.

Include, where possible:

- Affected version (tag, commit, or binary hash)
- Minimal reproducer (handler source, CLI flags, request)
- Observed vs. expected behavior
- Impact assessment (panic, isolation bypass, data exposure, etc.)

## Scope

In scope: the runtime (`packages/runtime/`), the zigts engine and virtual modules (`packages/zigts/`), the build-time tooling (`packages/tools/`), and the `zigttp deploy` client path.

Out of scope: the hosted control plane itself (`api.zigttp.dev`), which has its own reporting channel, and third-party extensions built with `zigttp-sdk`.

## Threat Model Summary

The high-level boundaries enforced by zigttp:

1. **Handler isolation.** Each request acquires a `Runtime` instance from a pool. Recycling under `reuse_unbounded` is only enabled when the compile-time contract proves `pure` + `deterministic`. Arena-escape and string-table audits run on every release; failures are logged via the runtime metrics (`arena_audit_failures`, `persistent_escape_failures`).

2. **Capability boundary.** Virtual modules declare `required_capabilities` in their `ModuleBinding`. The set is aggregated into the handler's contract and used by deployment policy to refuse mounts that exceed the granted surface. See [docs/internals/capability-audit.md](docs/internals/capability-audit.md) for the per-module review and known caveats (only `.runtime_callback` is enforced at module call time; other capabilities are policy-level).

3. **HTTP boundary.** Headers reject CRLF and NUL bytes before allocation (commit `6e65bb7`, `packages/runtime/src/http_types.zig`). Body size defaults to 1 MiB and is rejected with 413 on overflow. Static-file paths reject traversal (`..`), absolute paths, drive letters, and NUL bytes (`isPathSafe`); a second-line canonical-realpath check (`isCanonicalPathInsideRoot`) catches symlink escapes.

4. **WebSocket boundary.** RFC 6455 frame parsing is delegated to `std.http.Server.WebSocket`; runtime maps stdlib errors to wire-correct close codes (1002 for protocol error, 1009 for oversize). See [docs/internals/websocket-audit.md](docs/internals/websocket-audit.md) for the full checklist and the one documented gap (peer-sent close-code validation, deferred to W2).

5. **Attestation trust model.** Slice 1 (current beta): the JWS protected header carries the full Ed25519 public key. `zigttp verify <url>` validates the signature against that embedded key and prints the key fingerprint. This proves "the holder of this key signed these claims" - not "this key belongs to a specific publisher." For now, third-party verifiers should pin keys with `--trust-key <hex>`. Slice 2 adds identity binding via well-known endpoints. See [docs/roadmap/attest-slice-1.md](docs/roadmap/attest-slice-1.md) and [docs/roadmap/attest-slice-2.md](docs/roadmap/attest-slice-2.md).

6. **Self-extracting binary integrity.** The 32-byte trailer's CRC-32 is a corruption check, not a security check. The actual integrity boundary is the JWS attestation over `(bytecode_sha256, contract_sha256, policy_sha256)`. See [docs/internals/architecture.md - Self-Extracting Binary Trailer](docs/internals/architecture.md#self-extracting-binary-trailer).

7. **Bytecode integrity.** Only bytecode that passes `packages/zigts/src/bytecode_verifier.zig` is dispatched into the VM. The verifier validates opcodes, operand bounds, the constant pool, stack discipline, and jump targets before the first instruction executes. Untrusted bytecode (e.g. a tampered self-extracting payload) is rejected before bytecode execution.

8. **No runtime multipart parser.** `multipart/form-data` request bodies are passed to the handler unparsed. Handlers must parse boundaries themselves and reject malformed inputs; the runtime guarantees only `Content-Length` and total-body-size enforcement (default 1 MiB). Treat handler-side multipart parsing as untrusted-input handling.

## Related Documents

- [docs/threat-model.md](docs/threat-model.md) - public runtime and tooling threat model
- [docs/verification.md](docs/verification.md) - compile-time verification invariants
- [docs/internals/capability-audit.md](docs/internals/capability-audit.md) - per-module capability declaration audit
- [docs/internals/websocket-audit.md](docs/internals/websocket-audit.md) - RFC 6455 compliance audit
- [packages/zigts/src/module_binding.zig](packages/zigts/src/module_binding.zig) - capability enforcement surface
- [docs/design/zigttp_zigts_policy_wasm_spec.md](docs/design/zigttp_zigts_policy_wasm_spec.md) - deferred Wasm policy runtime design
