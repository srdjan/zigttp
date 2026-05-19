# Plan: Slice 3 Kickoff (Proof Receipts)

## Context

Slices 1 and 2 of the proof-receipts work have shipped. Slice 1 put a signed JWS on every HTTP response and gave third parties `zigttp verify <url>`. Slice 2 promoted the signing key from ephemeral to a persistent per-user identity at `~/.zigttp/attest/keypair.bin`, exposed the full attestation envelope at `GET /.well-known/zigttp-attest`, flipped `--attest` to default-on (with `--no-attest` opt-out), and added the "What can my caller see?" proof-card lens. Trust posture today: "this binary was signed by the holder of this key." Anyone can pin a fingerprint with `--trust-key`.

What's still missing for the v1 trust story:

1. **No hosted verifier.** Every consumer must install the zigttp CLI to validate an endpoint. There is no `verify.zigttp.dev` that a browser, a security reviewer, or a partner CI can hit.
2. **No badge.** Slice 2's `zigttp proofs badge` emits a static SVG describing the last local ledger entry; it has no relationship to a running attested endpoint.
3. **No rotation path.** The identity file is generated once and used forever. The well-known endpoint serves a single public key with no notion of "this is the active key, here is the previous one, here is the expected next one."

Slice 3 closes those three gaps as **architecture, not code**. The kickoff produces a single spec document. Implementation is a follow-on once the spec is approved.

Three scoping decisions are already settled (from this planning turn):

- Deliverable is **spec doc only**. No prototypes. No code touched.
- Verifier hosting posture: **we run it at `verify.zigttp.dev`**. The spec scopes what runs there, not whether it exists.
- **Transparency log is out of scope** for slice 3 (and not deferred to a later slice — explicitly dropped from the v1 trust story). Trust stays at "pinned fingerprint plus rotation chain." Revisit only if external demand appears.

## Deliverable

A single new file: `docs/roadmap/attest-slice-3.md`.

Format mirrors `docs/roadmap/attest-slice-2.md` exactly:

1. Front matter (Status / Date / Owner / Slice scope / Predecessor).
2. **Product Context** — what changes in the world after slice 3 ships, plus a success metric.
3. **Architecture Decision** — one subsection per topic (three subsections), each with the alternative considered explicit.
4. **Implementation Plan** — file-by-file shape per work item. Item-level so each can ship independently.
5. **Validation** — technical checks, performance gates, trust-chain test.
6. **Out of scope for this slice** — what slice 4+ inherits, plus tempting-but-rejected items.
7. **Critical files touched**.
8. **Open implementation questions** — five-minute decisions before coding begins.

## Scope of the spec (the three architecture sections)

### Section 1 — Hosted verifier at `verify.zigttp.dev`

The spec must resolve, with stated alternative:

- **Trust model.** Is the hosted verifier a trust anchor, a convenience layer, or a cache? The current CLI's posture is "the verifier extracts the key from the JWS and validates against it; pin with `--trust-key` for identity binding." The hosted service inherits the same posture by default, but the spec must say so explicitly and name what would *promote* it to a trust anchor (a curated key directory, signed advisories, etc.) so future slices have a hook.
- **Log structure.** What does the verifier write to disk or telemetry per request? Minimum schema: timestamp, queried hostname (or hash), result code (matches the CLI's exit codes 0–5), key fingerprint observed. Decide retention window. Decide whether the log is queryable (operator-only dashboard) or write-only.
- **Privacy posture for queried URLs.** Three live options to weigh: log full URL, log SHA-256 of URL, log eTLD+1 only. Default recommendation should match the project's "no data we wouldn't want public" instinct. State the user-visible privacy notice the service displays.
- **Service shape.** One endpoint that mirrors the CLI: `GET https://verify.zigttp.dev/v1/check?url=<encoded>` returning a JSON verdict equivalent to `zigttp verify --json`. Browser UI at `/` for human consumption. Both share the same backend.
- **Failure modes.** What happens when the target endpoint is down, slow, or returns a non-attested response. The spec must define timeouts and the verdict surface for each.

### Section 2 — Badge generator

Resolve **static SVG vs live endpoint vs both**, with reasoning. Concrete sub-decisions:

- If **static**: regenerated when? At `zigttp compile`? At `zigttp deploy`? Lives where in the repo? What does it claim — "last build was attested" or "the binary at path X carries proof receipts"? It cannot claim a deployed endpoint's status.
- If **live**: hosting (Cloudflare Worker fronting `verify.zigttp.dev`?), cache-control posture (shields.io conventions are 5-minute CDN cache), graceful degradation when the target is offline, query shape (`badge.zigttp.dev/<encoded-url>.svg` vs `verify.zigttp.dev/badge?url=…`).
- If **both**: how the user picks. The spec must give one-line guidance per audience (READMEs that travel with the repo → static; deploy dashboards that reflect live status → live).
- Visual language: lift from slice 2's existing badge SVG renderer in `packages/runtime/src/proofs_cli.zig:428` to keep style coherent.

### Section 3 — Key rotation story

Two halves: the **operator workflow** and what the **well-known endpoint advertises**.

- **Operator workflow.** A `zigttp keys rotate` CLI (mentioned as deferred in slice 2) — what does it do? Suggested shape: generate a new keypair, sign a transition statement with the old key naming the new key, archive the old key under `~/.zigttp/attest/archive/<fingerprint>.bin`, install the new key at the live path. The spec must define the transition-statement format (a small signed JSON document binding old fingerprint → new fingerprint → effective_at_unix).
- **Well-known endpoint advertisement.** Extend the slice-2 doc payload (currently `{ v, attest, contract, publicKey }`) with a `keyRotation` field listing previous keys (with their signed transitions) and optionally a next/expected key. Pinning consumers can follow the chain from a historical fingerprint to the current one without re-pinning manually.
- **Verifier behavior on rotation.** `zigttp verify --trust-key <hex>` must accept a key that matches the *current* public key OR any *prior* fingerprint reachable through a valid transition chain. The chain must be terminated (no cycles, monotonic `effective_at_unix`).
- **Threat model.** Compromised old key cannot sign a transition statement *backwards*; only forward. Lost old key means rotation is impossible without out-of-band trust re-establishment — the spec must name that as a known limitation, not paper over it.

## Critical files to reference while writing

Ground every decision in current state. Read at least:

- `docs/roadmap/attest-slice-1.md` and `docs/roadmap/attest-slice-2.md` — match tone, structure, depth.
- `packages/runtime/src/attest/identity.zig` — keypair lifecycle; line 12 names rotation as slice-3 work.
- `packages/runtime/src/attest/well_known.zig` — current doc shape; rotation field plugs in here.
- `packages/runtime/src/attest/envelope.zig` — JWS signing surface; transition statements should reuse the same primitive.
- `packages/runtime/src/verify_cli.zig` — current verifier flow; the hosted service mirrors it.
- `packages/runtime/src/proofs_cli.zig:428` onward — existing static SVG badge renderer.
- `packages/runtime/src/server.zig` — well-known route handling; header emission.

No new code touches these files in this slice. They exist to anchor design choices to working surface.

## Validation (acceptance criteria for the spec doc itself)

The spec is done when:

1. Each of the three architecture sections names a chosen approach AND the alternative that was considered, in the slice-2 style.
2. The trust-chain test (slice 2's section 4) has a slice-3 analogue: a rotation acceptance demo (operator rotates key on machine A; machine B's pinned old fingerprint still verifies via the transition chain; a forged transition signed by the wrong key fails closed).
3. The "Out of scope" section explicitly carries over: HSM custody, multi-key-per-environment, transparency log (note the dropped status, not deferred), federated verifier instances.
4. The "Open implementation questions" section lists the five-minute decisions: archive directory layout, transition statement JSON shape, badge URL scheme, hosted verifier rate-limit policy, retention window for verifier logs.
5. A separate reader, given only this spec and the slice-2 spec, can begin Item A implementation without coming back to ask "what did we decide about X." That is the slice-2 bar and the slice-3 bar.

## Out of scope for this kickoff

- Writing any Zig code, any frontend code, any Worker code.
- Standing up `verify.zigttp.dev` infra.
- Throwaway prototypes, even under `temp/`.
- Decisions about slice 4 content (HSM, federated verifiers, transparency log if it ever resurfaces).
- The proof-card UX changes that slice 3 will incidentally need (the spec can name them in its implementation plan section, but designing them is not blocking the spec).
