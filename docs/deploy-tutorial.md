# Deploy

`zigttp deploy` is the supported v1 deploy path. It produces a local self-contained binary, records the proof in `.zigttp/proofs.jsonl`, and never touches the network. `zigttp deploy --local` is the explicit spelling for the same target.

```
zigttp deploy
./.zigttp/deploy/<your-app-name>
```

That is the whole flow. No credentials, no Docker, no registry, no cloud account. Run the binary anywhere and it serves your handler with the same proven properties studio showed you in development.

## Proof receipts on the wire (`--attest`)

```
zigttp deploy --attest
```

Adds a slice-1 proof receipt to the build. The compiler signs the contract sha, bytecode sha, and rule-registry hash with a per-build Ed25519 key and embeds the JWS in the self-extracting binary. The running server then emits two response headers on every request:

- `Zigttp-Proofs: pure, read_only, injection_safe, ...` - the human-readable chip list.
- `Zigttp-Attest: <compact JWS>` - the signed envelope carrying the public key, claims, and signature.

Both values are precomputed once at startup; per-request cost is one `bufPrint` per header. Any consumer can validate the signature with `zigttp verify <url>`. The flag is opt-in for slice 1, default-off, and identity-bound signing is the slice-2 deliverable. See [docs/roadmap/attest-slice-1.md](roadmap/attest-slice-1.md) for the full design and trust model.

## Hosted control-plane deploy (preview)

`zigttp deploy --cloud` ships to the hosted Zigttp control plane (currently Northflank-backed). This path is in preview for v1.0: it works end-to-end but requires a Zigttp account, and the API surface may shift before general availability.

If you want to try it:

```
zigttp deploy --cloud
```

If you are not signed in yet, the CLI first prompts for a Zigttp access token directly in the terminal. The intended hosted flow is to create that token in the Zigttp web control plane, then paste it into the CLI. Press Enter on an empty token to fall back to browser-based device login. Future runs reuse the saved session, build the handler, push the image, and print the public URL.

If you want to sign in before the first deploy, use:

```
zigttp login
```

For scripting and CI, pass the token over stdin:

```bash
printf '%s\n' "$ZIGTTP_TOKEN" | zigttp login --token-stdin
```

## What it deploys

The handler, plus whatever sits in `.env` in the current directory as runtime variables. Nothing else. zigttp picks the rest out of the project:

- **Handler file**: the first match of `handler.ts`, `handler.tsx`, `handler.jsx`, `handler.js`, or the same paths under `src/`.
- **Service name**: the `name` field in `package.json`, then the basename of the git origin remote, then the current directory name. Slugified to lowercase with dashes.
- **Runtime environment**: `KEY=value` lines in `.env`. Missing file is fine. Malformed lines abort the deploy with a `path:line` diagnostic so you can find them.
- **Region**: `--region <name>` if you pass it, then the region from the previous deploy of this service, then `us-central`.

## What gets shipped

zigttp cross-compiles the handler to a Linux musl binary, packages it as an OCI image, and tags every image with compiler-proven facts from the handler contract: env var names, egress hosts, cache namespaces, route patterns, and boolean properties like `retry-safe`, `read-only`, and `idempotent`. The image manifest digest is content-addressed, and the CLI prints the digest alongside the public URL on success.

Before any upload happens, zigttp sends that handler contract to the control plane. If the contract expands risky capabilities, the control plane can block the deploy and return a review URL instead of credentials. The CLI prints the review summary, including which additions were already covered by reusable grants and which still need manual approval, then stops. After approval, re-run `zigttp deploy --cloud`.

To approve or reject a pending review from a terminal or CI job:

```bash
zigttp review <plan-id>
zigttp review <plan-id> --approve
zigttp review <plan-id> --approve --grant
zigttp review <plan-id> --reject
zigttp grants [project-name]
zigttp revoke-grant <grant-id>
```

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

## Updates

Re-run `zigttp deploy --cloud` any time. zigttp reuses the same service and updates it in place to the newly built image digest.

If something about the service has shifted since the last deploy (different scope, different region, different plan, or you removed a previously managed env var), the CLI prints a drift warning and exits with code 2 instead of mutating the service. Re-run with `--confirm` to acknowledge the warning and proceed:

```
zigttp deploy --cloud --confirm
```

`--confirm` rebinds and updates state; it never deletes the old service.

## Wait for ready

By default, `zigttp deploy --cloud` waits up to 120 seconds for the new service to come up before exiting. Pass `--no-wait` to skip the poll and return immediately after the deploy is accepted:

```
zigttp deploy --cloud --no-wait
```

Exit codes:

- `0` success
- `2` drift detected, re-run with `--confirm`
- `3` timed out waiting for the service to report ready
- `4` service failed to start

## Sign in and out

Credentials live at `~/.zigttp/credentials` after sign-in. To forget them:

```
zigttp logout
```

The next `zigttp deploy --cloud` will prompt for a fresh sign-in.

## Self-hosted control plane

By default the CLI talks to `https://api.zigttp.dev`. Point it at a self-hosted control plane by exporting `ZIGTTP_CONTROL_PLANE_URL`:

```
ZIGTTP_CONTROL_PLANE_URL=https://control.example.com zigttp deploy --cloud
```

The control plane provisions short-lived OCI registry credentials per deploy and forwards the image to the upstream provider, so the CLI never needs registry or provider tokens of its own.
