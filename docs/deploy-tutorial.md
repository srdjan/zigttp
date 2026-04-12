# Deploying your first handler

This walks through putting a zigttp handler on the public internet using
Render. About 15 minutes end to end. Northflank differs in a few env vars,
noted at the end.

## What you need before you start

- A working `zigttp` binary. If you built from source, it is at
  `./zig-out/bin/zigttp`. Add it to your `PATH` or invoke it by path.
- A handler file. `examples/handler/handler.ts` in this repo works. Anything
  that passes `zigttp check handler.ts` works.
- A GitHub account. The walkthrough uses GitHub Container Registry
  (`ghcr.io`) to store the container image zigttp builds. Free, private by
  default.
- A Render account with a paid plan. The Render free plan does not expose
  the API path this command uses. Sign up at render.com.

## Step 1: create a GHCR push token

zigttp needs a username and a token to push the image.

1. In GitHub, go to Settings → Developer settings → Personal access tokens
   → Tokens (classic). Click "Generate new token (classic)".
2. Name it `zigttp-deploy`. Check `write:packages`, `read:packages`,
   `delete:packages`. Save the token string; GitHub will not show it
   again.
3. Export the four OCI variables in your shell:

```bash
export ZIGTTP_OCI_REGISTRY=ghcr.io
export ZIGTTP_OCI_NAMESPACE=<your-github-username>
export ZIGTTP_OCI_USERNAME=<your-github-username>
export ZIGTTP_OCI_PASSWORD=<the-token-you-just-made>
```

Replace both `<your-github-username>` placeholders and the token
placeholder. For an organization repo, use the org slug as the namespace
instead of your username.

## Step 2: collect Render credentials

Render needs an API key, a workspace ID, and a plan tier.

1. Render dashboard → profile avatar → Account Settings → API Keys.
   Create a key and copy it.
2. Open your workspace settings. The URL contains a string like
   `wsp-abc123xyz`. That is your workspace ID.
3. Pick a plan. `starter` is the cheapest paid tier and is fine for a
   first deploy.

Export them:

```bash
export RENDER_API_KEY=<the-api-key>
export RENDER_WORKSPACE_ID=wsp-abc123xyz
export RENDER_PLAN=starter
```

## Step 3: dry-run the deploy

Always dry-run first. It does not push the image and it does not call
Render. It tells you exactly what a real run would do.

```bash
zigttp deploy --provider render --name demo --region oregon \
  --dry-run --json \
  examples/handler/handler.ts
```

You will see a large JSON document. The parts to look at:

- `"imageDigestRef"`. A string like
  `ghcr.io/yourname/demo@sha256:4a20f6...`. That is the exact immutable
  image the real deploy will push and Render will pull. Identical handler
  bytes always produce identical digests.
- `"providerPlan": { "action": "create", ... }`. Confirms Render will
  create a new service the first time.
- `"registryRequests"` and `"providerPlan.requests"`. The actual HTTP calls
  that will run.

If the command errors with "Missing required environment variables", it
lists every missing one at once, not one at a time. Set them and retry.

## Step 4: real deploy

Drop `--dry-run`. Keep `--json` if you want structured output instead of
a human summary.

```bash
zigttp deploy --provider render --name demo --region oregon \
  examples/handler/handler.ts
```

What happens in order:

1. zigttp verifies the handler in-process. Fast (sub-second).
2. It shells out to `zig build` to cross-compile the handler to a Linux
   binary.
3. It packages the binary as an OCI image entirely in memory.
4. It authenticates to `ghcr.io` and pushes the config, layer, and
   manifest blobs.
5. It calls the Render API to create a Web Service pointing at the image
   digest.
6. It writes `.zigttp/deploy-state.json` with the Render service id and
   other non-secret identifiers. Do not delete this file.

On success the command prints the service URL. Open it in a browser. You
should see your handler's response.

## Step 5: deploy an update

Change your handler, then run the exact same command. zigttp will:

1. Read `.zigttp/deploy-state.json` and find the existing service id.
2. Build and push a new image with a new digest.
3. Patch the Render service in place and trigger a rollout to the new
   digest.

You never bump a tag or pick a version string. Content addressing handles
versioning.

## When `--confirm` is required

Some changes are not silent updates. zigttp refuses them unless you pass
`--confirm`:

- You change `--region` on an existing service.
- You change `RENDER_PLAN` to a different tier.
- You change `RENDER_WORKSPACE_ID`.
- An env var that zigttp set on a previous run is missing on this run
  (zigttp tracks this in `managed_env_keys`).

When any of those happen, the command prints what would change and exits
non-zero. Rerun with `--confirm` if you meant the change. Even with
`--confirm`, zigttp v1 never deletes the old remote service; it only
rebinds local state.

## Adding runtime environment variables

For env vars your handler needs at runtime (database URLs, API keys):

1. Create `deploy.env` in the repo:
   ```
   DATABASE_URL=postgres://user:pass@host/db
   OPENAI_API_KEY=sk-...
   ```
2. Add `--env-file deploy.env` to the deploy command.

zigttp tracks which keys it set and leaves everything else on the Render
service alone. That matters if you also set vars through the Render
dashboard; zigttp will not overwrite them.

## Northflank instead of Render

Swap `--provider render` for `--provider northflank` and use Northflank's
env vars:

```bash
export NORTHFLANK_API_TOKEN=<token>
export NORTHFLANK_PROJECT_ID=<project-id>
export NORTHFLANK_PLAN_ID=nf-compute-20
```

`ZIGTTP_OCI_*` variables stay the same. Both providers pull from the same
pushed image.

## Troubleshooting

**"Missing required environment variables"**: the error lists every
missing name. Export them and rerun.

**"RegistryUnauthorized"**: your GHCR token is missing `write:packages`,
or `ZIGTTP_OCI_USERNAME` does not match the account that owns the token.
Regenerate.

**"BlobUploadFailed"**: usually a registry quota or transient network
error. Rerun.

**"RenderCreateFailed" with HTTP 402 or 403**: the `RENDER_PLAN` is not
available on your account, or your workspace needs billing set up. Pick
a plan the workspace supports.

**Service URL returns 502**: the container started but the handler is
not listening. Check the Render logs tab. The container runs zigttp
with `-p 3000`; if your handler overrides that or crashes on startup,
it will not respond.

**Second run creates a duplicate service instead of updating**: you
deleted `.zigttp/deploy-state.json`, or you are running from a different
directory than last time. The state file is path-keyed. Restore it from
git or pass `--confirm` to rebind.
