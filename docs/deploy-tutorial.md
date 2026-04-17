# Deploy

Put a `handler.ts` in your project directory. Test it locally with `zigttp serve handler.ts`. When it works, ship it:

```
zigttp-cli deploy
```

That is the whole command. No flags, no positional arguments, no config files, no cloud account, no registry to set up. If you are not signed in yet, the CLI first prompts for a Zigttp access token directly in the terminal. The intended hosted flow is to create that token in the Zigttp web control plane, then paste it into the CLI. Press Enter on an empty token to fall back to browser-based device login. Future runs reuse the saved session, build the handler, push the image, and print the public URL.

If you want to sign in before the first deploy, use:

```
zigttp-cli login
```

For scripting and CI, pass the token over stdin:

```bash
printf '%s\n' "$ZIGTTP_TOKEN" | zigttp-cli login --token-stdin
```

## What it deploys

The handler, plus whatever sits in `.env` in the current directory as runtime variables. Nothing else. zigttp picks the rest out of the project:

- **Handler file**: the first match of `handler.ts`, `handler.tsx`, `handler.jsx`, `handler.js`, or the same paths under `src/`.
- **Service name**: the `name` field in `package.json`, then the basename of the git origin remote, then the current directory name. Slugified to lowercase with dashes.
- **Runtime environment**: `KEY=value` lines in `.env`. Missing file is fine. Malformed lines abort the deploy with a `path:line` diagnostic so you can find them.
- **Region**: `--region <name>` if you pass it, then the region from the previous deploy of this service, then `us-central`.

## What gets shipped

zigttp cross-compiles the handler to a Linux musl binary, packages it as an OCI image, and tags every image with compiler-proven facts from the handler contract: env var names, egress hosts, cache namespaces, route patterns, and boolean properties like `retry-safe`, `read-only`, and `idempotent`. The image manifest digest is content-addressed, and the CLI prints the digest alongside the public URL on success.

Before any upload happens, zigttp sends that handler contract to the control plane. If the contract expands risky capabilities, the control plane can block the deploy and return a review URL instead of credentials. The CLI prints the review summary, including which additions were already covered by reusable grants and which still need manual approval, then stops. After approval, re-run `zigttp-cli deploy`.

To approve or reject a pending review from a terminal or CI job:

```bash
zigttp-cli review <plan-id>
zigttp-cli review <plan-id> --approve
zigttp-cli review <plan-id> --approve --grant
zigttp-cli review <plan-id> --reject
zigttp-cli grants [project-name]
zigttp-cli revoke-grant <grant-id>
```

## Updates

Re-run `zigttp-cli deploy` any time. zigttp reuses the same service and updates it in place to the newly built image digest.

If something about the service has shifted since the last deploy (different scope, different region, different plan, or you removed a previously managed env var), the CLI prints a drift warning and exits with code 2 instead of mutating the service. Re-run with `--confirm` to acknowledge the warning and proceed:

```
zigttp-cli deploy --confirm
```

`--confirm` rebinds and updates state; it never deletes the old service.

## Wait for ready

By default, `zigttp-cli deploy` waits up to 120 seconds for the new service to come up before exiting. Pass `--no-wait` to skip the poll and return immediately after the deploy is accepted:

```
zigttp-cli deploy --no-wait
```

Exit codes:

- `0` success
- `2` drift detected, re-run with `--confirm`
- `3` timed out waiting for the service to report ready
- `4` service failed to start

## Sign in and out

Credentials live at `~/.zigttp/credentials` after sign-in. To forget them:

```
zigttp-cli logout
```

The next `zigttp-cli deploy` will prompt for a fresh sign-in.

## Self-hosted control plane

By default the CLI talks to `https://api.zigttp.dev`. Point it at a self-hosted control plane by exporting `ZIGTTP_CONTROL_PLANE_URL`:

```
ZIGTTP_CONTROL_PLANE_URL=https://control.example.com zigttp-cli deploy
```

The control plane provisions short-lived OCI registry credentials per deploy and forwards the image to the upstream provider, so the CLI never needs registry or provider tokens of its own.
