# Technical Spec: Proof Receipts, Slice 1

Status: proposed.
Date: 2026-05-19.
Owner: TBD.
Slice scope: 1 of 3. Slices 2 and 3 are explicitly out of scope here.
Parent decision: `~/.claude/plans/analyze-the-core-features-steady-papert.md`.

## Product Context

Business objective: extend the zigttp thesis ("compile-time proof you can see") one step further, so the proof crosses the deploy boundary. Today, the proof card is visible only to the developer who ran the compiler. After this slice, every production response carries cryptographic evidence of its compile-time properties, and a new `zigttp verify <url>` subcommand lets anyone, anywhere, re-check those claims with zero coordination.

User problem: API consumers, partner integrators, security reviewers, MCP hosts, and compliance auditors currently have no way to independently verify "the response I just received was produced by a handler proven `injection_safe` and `no_secret_leakage`." They must trust the vendor's word and a vendor questionnaire. zigttp ships the proof; nothing ships the proof to them.

Success metric for this slice: end-to-end demo where `zigttp deploy --attest` produces a binary that serves a request whose response headers can be verified by `zigttp verify <url>` running on a third machine that has never seen the source. p99 added per-request latency under 100 microseconds. Slice 1 success is plumbing-correct, not adoption-correct; adoption metrics gate slice 2.

## Architecture Decision

Four design choices, each with an explicit alternative considered.

### 1. Signature algorithm: Ed25519

Chosen. Reasons: `std.crypto.sign.Ed25519` is in the stdlib (already used elsewhere in the codebase as `std.crypto.hash.sha2.Sha256` is in `proof_ledger.zig` and `contract_runtime.zig`), keys and signatures are tiny (32 byte public key, 64 byte signature), signing is deterministic, verification is fast and constant-time.

Alternative: ECDSA-P256. Wider hardware-token support (HSMs, smart cards), but heavier verification, signature malleability concerns, and the additional dependency surface buys nothing for slice 1's "self-contained binary" model.

### 2. Envelope format: compact JWS (RFC 7515 Section 7.1)

Chosen. Reasons: a single line of base64url-encoded text, parseable by every language ecosystem, well-understood security model, fits comfortably in an HTTP header. The compact form is `<protected_header>.<payload>.<signature>`, all base64url-encoded.

Alternative: ad-hoc binary envelope or SCITT-style CBOR receipts. Rejected: an ad-hoc format burdens every verifier with a custom parser. CBOR/COSE is the better long-term target for slice 3 once a transparency log lands, but is overkill here.

The protected header is `{"alg":"EdDSA","kid":"<pubkey_fingerprint>","typ":"zigttp-attest+jws"}`. The payload is the canonical JSON described in section 3 below.

### 3. Canonical hash input

The attestation payload commits to four things, in this exact order, separated by single newline bytes:

```
zigttp-attest-v1
sha256(contract.json bytes)
sha256(handler bytecode bytes)
sha256(policy_section_bytes)
```

The bytecode and policy hashes are the same values `contract_runtime.zig` already computes in `verifyArtifactHash` and `verifyPolicyHash`. The contract hash is added because the contract itself is the canonical surface of proven properties. The version prefix (`zigttp-attest-v1`) reserves future format evolution.

Property summary text (the human-readable `Zigttp-Proofs` header value) is derived from the same `Properties` struct in `contract_runtime.zig`; it is NOT signed independently. The signed root is the contract hash; the property summary is a presentation projection of the same source.

Alternative considered: sign the property summary string directly. Rejected because it duplicates trust: the contract hash already commits to every property and is shorter to express.

### 4. Trust model in slice 1

Honest constraint: slice 1 ships without a public key directory and without a transparency log. The verifier can therefore confirm "the binary that produced this response possessed the private key that signed this attestation," not "this binary belongs to the legitimate operator." That second guarantee is the explicit deliverable of slice 2 (well-known endpoint) and slice 3 (public verifier UI).

Slice 1 trust posture: the JWS protected header carries the full Ed25519 public key (32 bytes, base64url-encoded under the `jwk` field). The verifier extracts the key from the header, validates the signature against it, then prints the key fingerprint and the claims. A `--trust-key <hex_fingerprint>` flag (optional) lets a verifier pin a known key and fail closed on any other. This is sufficient to validate the cryptographic plumbing, deliver a demoable third-party verification flow, and reveal any integration sharp edges before slice 2 invests in identity binding.

Key custody during compile: ephemeral. `zigttp deploy --attest` (or `zigttp compile --attest`) generates a fresh Ed25519 keypair in process memory, signs the canonical input once, embeds the public key plus signature in a new self-extract section, then discards the private key. No file is written to disk that contains private key material. This is deliberate: it sidesteps key custody as a slice 1 problem and forces slice 2 to address it with a real design.

## Implementation Plan

Concrete file-by-file shape. Each item names the file, the change, and the existing function or struct it extends.

### New module: `packages/runtime/src/attest/`

Three files:

- `packages/runtime/src/attest/envelope.zig` - the canonical hash input, JWS encoding, and signing.
- `packages/runtime/src/attest/verify.zig` - parse a JWS, validate the signature, return claims.
- `packages/runtime/src/attest/header_strings.zig` - precompute the two response header values.

Key types:

```zig
// envelope.zig
pub const SigningInput = struct {
    contract_sha256: [32]u8,
    bytecode_sha256: [32]u8,
    policy_sha256: [32]u8,
};

pub const Claims = struct {
    version: []const u8,           // "zigttp-attest-v1"
    contract_sha256_hex: [64]u8,
    bytecode_sha256_hex: [64]u8,
    policy_sha256_hex: [64]u8,
    compiler_version: []const u8,  // build constant
    signed_at_unix: i64,
    property_summary: []const u8,  // comma-separated chip list
    routes_count: usize,
    capability_hash_hex: [64]u8,
};

pub const Envelope = struct {
    pub_key: [32]u8,
    signature: [64]u8,
    jws_compact: []u8,   // owned, contains protected_header.payload.signature
    claims: Claims,
};

pub fn sign(allocator: std.mem.Allocator, input: SigningInput, claims: Claims) !Envelope;
```

Signing implementation: produce the canonical bytes, run `std.crypto.sign.Ed25519.KeyPair.generate()`, sign the SHA-256 of the canonical bytes, base64url-encode the protected header (containing the `jwk` for the fresh public key) and the claims payload (JSON), join with dots, and emit.

Verifying implementation: split on dots, base64url-decode the three parts, parse the protected header to extract the embedded public key, validate the signature against the JOSE signing input (`protected_header_b64url + "." + payload_b64url`). On success, return the parsed claims.

Header string precompute (`header_strings.zig`):

```zig
pub const HeaderStrings = struct {
    attest_value: []const u8,  // the compact JWS, owned
    proofs_value: []const u8,  // "pure,read_only,injection_safe,...", owned
};

pub fn build(
    allocator: std.mem.Allocator,
    contract: *const contract_runtime.ValidatedRuntimeContract,
    envelope: *const envelope.Envelope,
) !HeaderStrings;
```

`proofs_value` is produced by iterating `contract.properties()` and joining the `true` field names. Field-to-string mapping mirrors the snake-case used in the existing `Properties` struct in `contract_runtime.zig`. Field order is fixed and stable across runs (declaration order of `Properties`) so the value is byte-identical for identical contracts.

### Extension: `packages/runtime/src/self_extract.zig`

Add a new section type and one flag bit. No backwards-compatibility break: parsers already skip unknown section types (`else => {}` in `parsePayload`).

```zig
pub const Section = enum(u8) {
    bytecode = 1,
    deps = 2,
    contract = 3,
    policy = 4,
    metadata = 5,
    attestation = 6,  // NEW: holds the JWS envelope bytes
};

// Flags
// bit 0: has_contract (existing)
// bit 1: has_attestation (NEW)
```

Update `create()` to accept an optional `attestation: ?[]const u8` parameter and serialize it as a section when present. Update `Payload` to carry `attestation_jws: ?[]const u8`, and have `parsePayload` populate it. The trailer format itself is unchanged; format_version stays at 1 because the trailer is still parseable by older runtimes (they skip the unknown section).

### Extension: `packages/runtime/src/contract_runtime.zig`

No type changes. Add one helper:

```zig
pub fn extractPolicyBytes(payload: *const self_extract.Payload) ![]const u8;
```

It serializes the policy via the same path `self_extract.serializePolicy` already uses, so the attestation hash is computed over the bytes that physically live in the binary. This is the same trust posture as `verifyArtifactHash` in this file today.

### Extension: server-side header emission

The runtime's HTTP server currently emits responses without these headers. Two new headers are added on every HTTP response (not WebSocket frames):

- `Zigttp-Proofs: pure, read_only, injection_safe, ...`
- `Zigttp-Attest: <compact_jws>`

Both values are precomputed once during `contract_runtime` startup (after `validate()` succeeds) and held in a `HeaderStrings` allocated for the runtime lifetime. Per-request cost is one `writer.print` per header; no parsing, no crypto, no allocation. When `--attest` was not passed at compile time, the `Payload.attestation_jws` is `null`, `HeaderStrings` is `null`, and both writes are skipped.

The integration point is wherever the server writes response headers today. From the exploration, this lives near `packages/runtime/src/server.zig`. The change is: after the existing `Content-Type` line, conditionally write the two new lines.

### CLI surface: `zigttp deploy --attest` and `zigttp compile --attest`

Add a single flag to both subcommands in `dev_cli.zig`. The flag plumbs through to `buildArtifact()`, which orchestrates the contract extraction and self-extract creation. After the contract JSON and policy bytes are finalized, `buildArtifact()`:

1. Computes the three SHA-256 inputs.
2. Builds the canonical `Claims` struct (compiler version from `build.zig` constant; property summary by walking the contract).
3. Calls `attest.envelope.sign` to get the JWS.
4. Passes the JWS bytes to `self_extract.create` as the optional `attestation` argument.

The `--attest` flag is opt-in in slice 1. Default behavior is unchanged. This is non-negotiable for the slice: zero risk of breaking existing deploys.

### CLI surface: `zigttp verify <url>`

New subcommand. Live next to the existing proofs subcommands but at the top level since "verify" is a primary verb. Either:

- New file `packages/runtime/src/verify_cli.zig` with `pub fn run(...)`, dispatched from `dev_cli.zig`.
- Or, since `proofs_cli.zig` already has a `.verify` subcommand for bundle verification, mount this as `zigttp verify <url>` distinct from `zigttp proofs verify <dir>`. Recommend the former, so `verify` reads as a top-level operation.

Behavior:

1. Parse `<url>`. Reject non-http(s) schemes.
2. Issue a single `HEAD` request (or `OPTIONS`, but `HEAD` is more universal). Use the `fetch` infrastructure already imported elsewhere in the codebase; if there is no Zig-side HTTP client wired into the runtime CLI, this slice adds a minimal `std.http.Client` call.
3. Read `Zigttp-Attest` and `Zigttp-Proofs` headers. If absent, print "endpoint is not attested" and exit 2.
4. Decode the JWS, extract the embedded `jwk` public key, validate the signature.
5. Print the claims in the same chip layout the proof card uses, plus the key fingerprint (SHA-256 of the public key bytes, lower hex, first 16 chars).
6. If `--trust-key <hex>` was passed and the fingerprint does not match, exit 3. If `--json` was passed, emit the structured claims instead of human output (for CI use).

Error taxonomy mirrors `proofs_cli.zig:isExpectedUserError`. Add the new tags: `error.NotAttested`, `error.SignatureInvalid`, `error.KeyMismatch`, `error.UnsupportedScheme`.

### Build system

`build.zig` needs:

- A new `compiler_version` build constant exposed via `@import("build_options")` or similar. The constant is the short git sha plus a stable tag, set at build time. The attestation embeds it as plain text under `claims.compiler_version`.
- Wire the new `attest/` module into the runtime binary build, the developer CLI build, and the `test` step. Tests live alongside source per project convention.

## Validation

### Technical checks

Three layers of tests, all under `zig build test`.

Unit tests in `attest/envelope.zig`:

1. Round-trip: sign a known input, verify the same envelope, confirm claims are byte-equal.
2. Tamper resistance: flip a single bit of the payload, confirm verify fails with `error.SignatureInvalid`.
3. Wrong key: verify with a different public key, confirm failure.
4. Stable output: signing the same input twice with the same keypair produces identical signatures (Ed25519 is deterministic). This protects against accidental nondeterminism if anyone replaces the algorithm.
5. Field order stability: the property summary for a fixed `Properties` struct is byte-identical across runs.

Integration tests in `self_extract.zig`:

6. Existing round-trip tests (`"roundtrip: serialize and parse payload"`) keep passing with the new section enum and flag bit.
7. New test: round-trip a payload with `attestation_jws` set, confirm `parsePayload` returns the same bytes.
8. New test: an older payload without the attestation section parses unchanged (`attestation_jws` is null).

End-to-end test in `dev_cli` tests:

9. `zigttp compile --attest examples/handler/handler.ts handler.zigttp`, then read trailer, confirm attestation section exists, parse JWS, validate.
10. Run the produced binary as a server (`zigttp run handler.zigttp -p 0`, capture port), hit `GET /`, confirm `Zigttp-Attest` and `Zigttp-Proofs` headers are present and parse cleanly.

### Product metrics and review window

This slice is plumbing; product metrics gate slice 2, not slice 1. The validation gate for slice 1 is:

- All tests pass under `zig build test`.
- A 60-second `zigttp dev --attest` session shows no measurable latency regression in the existing benchmark (`zig build bench`). Target: p99 added per-request latency under 100 microseconds, measured. If above, investigate before merge.
- One end-to-end demo: spin up an attested handler on the dev machine, run `zigttp verify http://localhost:PORT` from a second terminal, capture the human-readable output. Attach to the PR.

### Hypotheses tested

This slice tests two falsifiable hypotheses:

H1: signing per build adds negligible compile-time overhead. Threshold: under 50 milliseconds added to `zigttp deploy` on the example handler. If above, the envelope encoder needs a profile pass before slice 2 lands.

H2: per-request runtime cost of two precomputed headers is below the noise floor of the existing benchmark. Threshold: p99 within 1 percent of the unattested baseline. If above, the integration point is wrong (most likely an unnecessary allocation per response).

## Out of scope for this slice

Explicitly deferred to slice 2:

- `/.well-known/zigttp-attest` endpoint.
- Default-on attestation (slice 1 stays opt-in; slice 2 flips the default).
- Studio "What can my caller see?" lens.

Explicitly deferred to slice 3:

- Public verifier UI at a hosted URL.
- README badge generation tied to live attestation status.
- Any form of public transparency log.

Also explicitly out of slice 1, even though tempting:

- Persistent signing keys (per-org or per-project). Slice 1 uses ephemeral per-build keys. Identity binding is a slice 2 problem.
- Reproducible-build verification. The attestation says "the binary that produced this response had this contract hash," not "this source code produced this binary." Reproducibility is a complementary slice 2 candidate.
- Differential replay on deploy. Independent feature, not in the attestation arc.

## Critical files touched

- `packages/runtime/src/attest/envelope.zig` (new)
- `packages/runtime/src/attest/verify.zig` (new)
- `packages/runtime/src/attest/header_strings.zig` (new)
- `packages/runtime/src/self_extract.zig` (section enum, flags, optional parameter in `create`, optional field in `Payload`)
- `packages/runtime/src/contract_runtime.zig` (one helper to surface policy bytes)
- `packages/runtime/src/server.zig` or whichever module emits response headers today (two conditional `writer.print` calls)
- `packages/runtime/src/dev_cli.zig` (`--attest` flag plumbing on `compile` and `deploy`)
- `packages/runtime/src/verify_cli.zig` (new) plus dispatch entry
- `build.zig` (compiler_version constant)

## Open implementation questions

These need a five-minute decision before coding begins; none changes the architecture.

1. Exact integration point for response header emission in `server.zig`. Best resolved by reading the function that builds the existing response headers.
2. Whether `zigttp verify <url>` reuses the runtime's `fetch` virtual module client or pulls in `std.http.Client` directly. Recommend `std.http.Client` for simplicity; the CLI does not need the proven egress model the runtime enforces.
3. Whether the JWS protected header carries the public key as `jwk` (JSON Web Key, the spec-conformant form) or as a bare `x` parameter (compact, less standard). Recommend `jwk` for interop.
4. Whether the property summary in `Zigttp-Proofs` includes only `true` properties or also explicit `false` markers. Recommend `true` only, to keep the header short and align with the proof card's "chip lit / chip dark" semantics.
