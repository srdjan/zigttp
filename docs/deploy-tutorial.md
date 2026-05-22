# Deploy

`zigttp deploy` is the supported v1 deploy path. It produces a local self-contained binary, records the proof in `.zigttp/proofs.jsonl`, and never touches the network. `zigttp deploy --local` is the explicit spelling for the same target.

```
zigttp deploy
./.zigttp/deploy/<your-app-name>
```

That is the whole flow. No credentials, no Docker, no registry, no cloud account. Run the binary anywhere and it serves your handler with the same proven properties studio showed you in development.

## Proof receipts on the wire

Attestation is default-on. Every `zigttp deploy` signs the contract sha, bytecode sha, and rule-registry hash with the persistent Ed25519 identity at `~/.zigttp/attest/keypair.bin` (minted on first use, mode 0600) and embeds the JWS in the self-extracting binary. The running server emits two response headers on every request:

- `Zigttp-Proofs: pure, read_only, injection_safe, ...` - the human-readable chip list.
- `Zigttp-Attest: <compact JWS>` - the signed envelope carrying the public key, claims, and signature.

It also serves `GET /.well-known/zigttp-attest` with the full attestation envelope plus the embedded contract surface as cacheable JSON (`Cache-Control: public, max-age=3600`, ETag, 304 on `If-None-Match`), so security scanners and registry crawlers can fetch the proof without calling every handler route.

Pass `--no-attest` to skip signing for a single build (e.g. air-gapped or privacy-restricted deploys). The legacy `--attest` flag from slice 1 still works but warns once: it is now the default. Any consumer can validate the signature with `zigttp verify <url>` from any third-party machine. See [docs/roadmap/attest-slice-2.md](roadmap/attest-slice-2.md) for the full design and trust model.

## Hosted cloud deploy

A hosted control-plane deploy is in development and deferred from
v0.1.0-beta. `zigttp deploy --cloud`, `zigttp login`, and the related
account commands are not available in this release. The supported path
is the self-contained binary above - run it on any host, container, or
FaaS target.

## Proof review card

After every successful `zigttp deploy`, the CLI prints a review card showing what the compiler proved about this version: contract sha, proof level, proven properties, the route/env/egress/cache/capability surface, and a verdict against the previous deploy. The verdict words (`safe`, `safe_with_additions`, `breaking`) are the same ones `zigttp dev --watch --prove` already uses, so the language matches between dev and deploy.

The CLI also appends the row to `.zigttp/proofs.jsonl` so the timeline survives across deploys. Browse with:

```bash
zigttp proofs list                              # recent deploys and live-reload swaps
zigttp proofs show HEAD                         # re-render the card from the latest entry
zigttp proofs diff HEAD~1 HEAD                  # what changed between the last two entries
zigttp proofs export --format md > receipt.md   # shareable receipt for a PR
zigttp proofs export --format svg > badge.svg   # verdict badge for a README
```

Refs accept `HEAD`, `HEAD~N`, or a contract sha prefix. The ledger persists only contract-derived identifiers (env var names, egress hosts, cache namespaces, route patterns, capability names, the contract sha, boolean property flags); no env values, tokens, or PII enter the file.
