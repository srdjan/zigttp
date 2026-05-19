# Technical Spec: Proof Receipts, Slice 2

Status: proposed.
Date: 2026-05-19.
Owner: TBD.
Slice scope: 2 of 3. Slice 3 (public verifier UI, transparency log, README badges) explicitly deferred.
Parent decision: `~/.claude/plans/analyze-the-core-features-steady-papert.md`.
Predecessor: [attest-slice-1.md](attest-slice-1.md).

## Product Context

Slice 1 landed signed proof receipts on every HTTP response, but the trust posture was honestly "whoever held this private key signed these claims," not "this key belongs to the legitimate operator." Slice 2 closes that gap. After slice 2:

- A persistent Ed25519 keypair represents an organisational identity, not a per-build ephemeral key. Multiple deploys from the same developer or team produce attestations under the same key. Verifiers can pin once and trust every future deploy from that origin.
- A `/.well-known/zigttp-attest` endpoint serves the full attestation envelope plus the contract surface as cacheable JSON. Auditors and API marketplaces can fetch the doc without issuing a request through every handler endpoint.
- `--attest` is the default. Every fresh deploy carries proof receipts unless the operator explicitly opts out with `--no-attest`.
- The proof-card UX adds a fourth lens, "What can my caller see?", showing the live attestation envelope, response-header text, and a copy-pasteable `zigttp verify <url>` command. Closes the loop: the developer sees exactly what an external verifier sees.

Success metric for this slice: end-to-end demo where a team pins their organisation key fingerprint once, ships a new build, and a third party's CI rejects any deployed endpoint whose attestation key does not match the pinned fingerprint.

## Architecture Decision

Four design choices, each with the alternative considered.

### 1. Key custody: per-user signing key under `~/.zigttp/`

Chosen. Reasons: matches the existing `~/.zigttp/credentials` pattern; one identity across every project the operator deploys; survives `git clean` and project-level state resets; private key never enters the repository; trivial to back up to a password manager. The file (`~/.zigttp/attest.key`) holds 64 bytes (32-byte Ed25519 seed + 32-byte public key) with mode 0600. Generated on first use of `--attest` if missing.

Alternative considered: per-project key under `.zigttp/attest-key.bin`. Cleaner project isolation but ties identity to project files (which may be cloned, forked, or copied between machines). Per-user wins for the v1 case of "this is my deploy key." Org-tier rotation and per-project override land in slice 3.

### 2. Well-known endpoint: cacheable JSON served from precomputed bytes

Chosen. Path: `GET /.well-known/zigttp-attest`. Body shape:

```json
{
  "v": "zigttp-attest-v1",
  "attest": "<compact JWS>",
  "contract": { ...the full embedded contract.json... },
  "publicKey": { "kty": "OKP", "crv": "Ed25519",
                 "x": "<base64url>", "kid": "<sha256 fingerprint>" }
}
```

Built once at startup from the embedded JWS, the parsed contract, and the embedded public key. Stored as one owned byte slice on `Server`. Served via a fast-path string match against the request path before the JS runtime is consulted, so per-request cost is one `memcmp` plus one `write`.

Headers: `Content-Type: application/json`, `Cache-Control: public, max-age=3600`, `ETag: "<sha256 of body>"`. The 3600s TTL matches a typical CDN edge cache and is configurable later if needed.

Alternative considered: serve the doc only through the existing proof-card route, or only via a CLI subcommand that prints the same JSON. Rejected because the value proposition of a well-known doc is exactly that any HTTP client can fetch it without knowing zigttp internals. The `.well-known/` prefix is RFC 8615 turf and reads correctly to security scanners and registry crawlers.

### 3. Default-on attestation: opt out with `--no-attest`

Chosen. Reasons: the cost is one-time keypair generation plus a 1.2 KB JWS in every binary. There is no per-request work that opts-in escapes. Making it default-on means every fresh deploy carries proof receipts, which is the whole point of the slice. Operators with policy reasons to suppress (e.g., privacy concerns about leaking compiler version in headers) pass `--no-attest`. The flag name flips so the rare case is the typed flag, not the universal case.

Migration: existing `zigttp compile --attest` calls keep working; the flag becomes a no-op (warn once, then silent). `zigttp compile --no-attest` produces a slice-0-shaped binary identical to today's default.

Alternative considered: keep slice 1's opt-in default and require a `.zigttp/config.toml` flag to flip it. Rejected because zigttp has no `.zigttp/config.toml` today, and adding a config file just to flip one bool is more API surface than the deliverable.

### 4. Studio lens: fourth tab "What can my caller see?"

Chosen. Adds a `.caller_view` variant to the existing rotating proof-card lenses (`.properties`, `.trade`, `.handover`). Renders four blocks in the TUI:

1. Response header preview (the exact text `Zigttp-Proofs:` and `Zigttp-Attest:` lines the runtime emits).
2. `/.well-known/zigttp-attest` URL with a "Try: `curl` …" hint.
3. A copy-pasteable `zigttp verify <local-url>` command pinned to the running server's port.
4. The public-key fingerprint, plus a one-line note about how to pin it (`--trust-key <hex>`).

The same view is mirrored to the Studio browser overlay via the existing SSE pipe, matching the slice 1 polish work (`feat(studio): mirror terminal proof card in browser overlay`).

## Implementation Plan

Concrete file-by-file shape. Four work items; each can ship independently.

### Item A: persistent identity key

New module `packages/runtime/src/attest/identity.zig`:

```zig
pub const KeyError = error{
    HomeDirUnavailable,
    KeyFileMalformed,
    PermissionDenied,
} || std.fs.File.OpenError || std.fs.File.WriteError;

pub const KeySource = enum { generated, loaded };

pub const SignerIdentity = struct {
    key_pair: std.crypto.sign.Ed25519.KeyPair,
    fingerprint_hex: [64]u8,
    source: KeySource,
};

/// Returns the persistent identity for this user. Generates a fresh
/// keypair, writes it to `~/.zigttp/attest.key` with mode 0600, and
/// returns it on first use. On subsequent calls, loads from disk.
pub fn loadOrCreate(allocator: std.mem.Allocator) KeyError!SignerIdentity;
```

File format at `~/.zigttp/attest.key`: 64 raw bytes (seed + public key). No header, no encoding. Permissions 0600 enforced via `std.posix.chmod` after creation; load path rejects modes wider than user-rw with a clear error.

`dev_cli.zig:buildAttestationJws` swaps the ephemeral `fillCsprngSeed` + `generateDeterministic` block for one call to `identity.loadOrCreate`. Slice 1's `fillCsprngSeed` helper is moved into `identity.zig` as the generator path.

### Item B: well-known endpoint

Extension to `server.zig`:

1. Build the well-known doc once in `Server.start` after attestation_headers is built. Store as `Server.well_known_attest_body: ?[]const u8`.
2. Build helper in a new file `packages/runtime/src/attest/well_known.zig`:

```zig
pub const Doc = struct {
    body: []const u8,
    etag: [64]u8,

    pub fn deinit(self: *Doc, allocator: std.mem.Allocator) void;
};

pub fn build(
    allocator: std.mem.Allocator,
    contract_json: []const u8,
    attestation_jws: []const u8,
    public_key: [32]u8,
) !Doc;
```

3. Route handling: at the top of the request dispatch in `server.zig` (before JS runtime invocation), check `if (std.mem.eql(u8, req.path, "/.well-known/zigttp-attest"))` and respond with the precomputed body, ETag, and cache headers. Handle `If-None-Match` by returning 304 when the ETag matches.

The body is content-addressed: the ETag is the SHA-256 hex of the body. Identical builds produce identical bodies and ETags, which means CDNs and verifiers can cache aggressively.

### Item C: default-on flag flip

In `dev_cli.zig`:

1. Change the three `attest_requested` initializers from `false` to `true`.
2. Replace the single `--attest` branch in each arg loop with a two-flag pattern: `--no-attest` (sets to false) plus a deprecated `--attest` (sets to true with a one-time warning printed to stderr on first call). The deprecation warning fires once per process to avoid spamming.
3. Update each `printXxxHelp()` to advertise `--no-attest` instead of `--attest`.

The `attest_flag` constant in `dev_cli.zig` is replaced with two: `attest_flag_legacy = "--attest"` and `no_attest_flag = "--no-attest"`.

### Item D: Studio "What can my caller see?" lens

Two pieces, mirroring the existing proof card pattern:

1. `proof_card_tui.zig`: add `.caller_view` to the `Lens` enum, rotate it into the Tab-cycle alongside `.properties`, `.trade`, and `.handover`. New render function builds the four blocks listed in the architecture section.
2. Studio mirror (browser overlay): the existing SSE pipe already broadcasts the active lens content; the new lens flows through without server-side changes beyond rendering the new template.

The browser overlay's tab bar gains a "Caller View" pill. Persistence of the active lens across reloads (already implemented for the three existing tabs) extends to the new one without code changes.

## Validation

### Technical checks

Per item:

**Identity (Item A)**:
1. Round-trip: `loadOrCreate` on a fresh `~/.zigttp/` creates a 64-byte file, mode 0600; second call returns the same fingerprint.
2. Permission guard: a file with mode 0644 is rejected with `error.PermissionDenied` and a clear remediation message.
3. Malformed file: a 32-byte file (incomplete) is rejected with `error.KeyFileMalformed`.
4. End-to-end: two consecutive `zigttp compile --attest` runs of the same source produce binaries with identical key fingerprints (deterministic identity).

**Well-known (Item B)**:
5. Doc build: `well_known.build` produces deterministic bytes for fixed inputs.
6. ETag stability: identical inputs produce identical ETag.
7. HTTP round-trip: GET `/.well-known/zigttp-attest` returns 200 with the doc; If-None-Match with the ETag returns 304.
8. JSON shape: response parses cleanly as JSON and contains the four required top-level keys (`v`, `attest`, `contract`, `publicKey`).

**Default-on (Item C)**:
9. Default behavior: bare `zigttp compile handler.ts -o ./out` produces an attested binary (Section.attestation present).
10. Opt-out: `zigttp compile handler.ts -o ./out --no-attest` produces an unattested binary (Section.attestation absent).
11. Legacy flag: `--attest` still works but logs a one-time deprecation warning.

**Studio lens (Item D)**:
12. TUI: Tab-cycle rotates through four lenses including `.caller_view`.
13. Browser: Studio mirror displays the new lens with all four blocks.

### Performance gates

- Item A: identity load on cold start adds under 5 ms (one file read of 64 bytes).
- Item B: well-known response p99 under 200 microseconds (no allocation, no JSON serialisation on the request path - bytes are precomputed).
- Item C: no per-request impact (default flip changes compile-time orchestration only).
- Item D: no runtime cost outside the dev workflow.

### Trust-chain test

The slice-2 acceptance demo:

1. On machine A, run `zigttp compile --no-attest handler.ts -o ./build/v1`. Extract the public-key fingerprint with `zigttp verify` against a local run (or query `~/.zigttp/attest.key` directly).
2. On machine B (no shared state), pin that fingerprint and run `zigttp verify https://prod-url --trust-key <fp>`. Confirm exit 0.
3. Rebuild on machine A with a NEW handler. Confirm the fingerprint is unchanged.
4. Generate a fresh key on machine C (different operator). Deploy. Confirm machine B's `zigttp verify --trust-key <fp>` exits 4 (KeyMismatch) for machine C's URL.

This proves identity persists across builds from the same operator and differs across operators.

## Out of scope for this slice

Explicitly deferred to slice 3:

- Public verifier UI hosted at a known URL (e.g., `verify.zigttp.dev`).
- README badge generator tied to live attestation status.
- Public transparency log (Sigstore/Rekor-style append-only registry).
- Key rotation CLI (`zigttp keys rotate`).
- HSM or hardware-backed key custody.
- Multi-key support (e.g., per-environment keys for staging vs production).

Out of slice 2 even though tempting:

- A second well-known path (e.g., `/.well-known/zigttp-attest.jws` returning just the bare JWS) for tools that prefer the unwrapped form. Cleaner shape for slice 3.
- Mutual-TLS or auth on the well-known endpoint. The doc is public by definition.

## Critical files touched

- `packages/runtime/src/attest/identity.zig` (new)
- `packages/runtime/src/attest/well_known.zig` (new)
- `packages/runtime/src/attest/envelope.zig` (no changes; the existing `sign` accepts any keypair)
- `packages/runtime/src/dev_cli.zig` (default-on flip; identity load replaces fillCsprngSeed; help text updates)
- `packages/runtime/src/server.zig` (well-known route handling; new field for the precomputed body)
- `packages/runtime/src/runtime_cli.zig` (no changes; serveAppended already plumbs attestation_jws)
- `packages/runtime/src/proof_card_tui.zig` (new lens variant; rotation; render)
- Tests live alongside source per project convention.

## Open implementation questions

Five-minute decisions before coding begins; none changes the architecture:

1. Order of work. Recommend: Item A (identity, foundation), then Item B (well-known, public surface), then Item C (default-on, gates the migration), then Item D (Studio lens, polish). Each can land in its own commit.
2. `~/.zigttp/attest.key` vs `~/.zigttp/attest/keypair.bin` (the `attest/` subdirectory leaves room for future per-environment keys without a path rename). Recommend the subdirectory form.
3. Well-known `Cache-Control: max-age=3600` vs `no-cache` with an ETag. Recommend max-age=3600 because the doc is content-addressed; if a build changes, the binary changes, the response changes, the ETag changes; clients on cached copies just re-validate.
4. Deprecation message format for `--attest`. Recommend: `note: --attest is now the default; remove it from your invocation or pass --no-attest to opt out.` printed once per process to stderr.
5. Whether to add an attestation summary to `zigttp deploy`'s success card. Recommend yes - one line showing the key fingerprint at the bottom of the existing review card, gated on attestation being enabled. Low cost, high signal: every successful deploy logs the identity it just shipped.
