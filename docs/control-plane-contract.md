# Control Plane Contract

`zigttp deploy` and `zigttp login` are client-side binaries that talk to a hosted control plane. The plane itself is not in this repo; the wire contract below is reconstructed from the deploy subsystem source. To run a self-hosted plane via `ZIGTTP_CONTROL_PLANE_URL`, implement the endpoints, payloads, and status codes here.

## Source of truth

Everything below is derived from the following files. If this document and the code disagree, the code wins and this document is stale:

- [`packages/runtime/src/deploy/control_plane.zig`](../packages/runtime/src/deploy/control_plane.zig) - HTTP client, URL construction, response parsing
- [`packages/runtime/src/deploy/auth.zig`](../packages/runtime/src/deploy/auth.zig) - credential file format and location
- [`packages/runtime/src/deploy/builder.zig`](../packages/runtime/src/deploy/builder.zig) - cross-compilation pipeline
- [`packages/runtime/src/deploy/types.zig`](../packages/runtime/src/deploy/types.zig) - provider enum, arch enum, deploy result shape
- [`packages/runtime/src/deploy/northflank_adapter.zig`](../packages/runtime/src/deploy/northflank_adapter.zig) - the one provider backend currently wired in
- [`packages/runtime/src/deploy.zig`](../packages/runtime/src/deploy.zig) - proven-fact label assembly for OCI images
- [`docs/deploy-tutorial.md`](deploy-tutorial.md) - user-facing walkthrough

## Base URL

```
default:  https://api.zigttp.dev
override: $ZIGTTP_CONTROL_PLANE_URL
```

Defined as `default_base_url` in `control_plane.zig:6`. The client dupes the env var verbatim with no trailing-slash normalization, so self-hosted deployments must match the scheme and host exactly.

All endpoints are `POST` and use JSON bodies. Responses are JSON. The client reads the `status` numerically; no `Retry-After` or backoff headers are consulted.

## Authentication

### Credentials file

Path: `~/.zigttp/credentials` (hardcoded in `auth.zig:15`). Created with mode `0600`. Format:

```json
{
  "token": "<opaque-string>",
  "email": "user@example.com"
}
```

The `email` field is optional and purely informational. Missing `HOME` produces `error.HomeDirMissing`. Missing file produces `error.NotSignedIn`, which the CLI catches and turns into an interactive token prompt (`first_run.zig`).

### Device code login flow

Used by `zigttp login` when the user does not paste a token directly. Two endpoints:

**`POST /v1/auth/device/start`**

Request body: `{}` (empty object). No auth header.

Response 200:

```json
{
  "deviceCode": "ABCD-1234",
  "verificationUrl": "https://zigttp.dev/cli/authorize?code=ABCD",
  "intervalSeconds": 5,
  "expiresInSeconds": 600
}
```

Missing or non-integer `intervalSeconds` falls back to 5. Missing or non-integer `expiresInSeconds` falls back to 600. Both `deviceCode` and `verificationUrl` are required; their absence returns `error.InvalidChallengeResponse`.

**`POST /v1/auth/device/poll`**

Request body: `{"deviceCode": "<code-from-start>"}`. No auth header.

Status code is the primary signal:

| Status | Meaning |
|---|---|
| 200 | User approved. Body must contain `{"token": "...", "email": "..."}` (email optional). |
| 202 | Still pending. Client sleeps `intervalSeconds` and polls again. |
| 410 | Challenge expired. Client aborts and restarts the flow. |
| 403 | User denied. Client aborts. |
| >=400 (other) | `error.ControlPlaneError`. |

### Token verification

**`POST /v1/auth/token/verify`**

Header: `Authorization: Bearer <token>`. Request body: `{}`. Empty response body is valid and produces `TokenIdentity{email = null}`. A non-empty body must be a JSON object with an optional `email` field. Status codes:

| Status | Meaning |
|---|---|
| 200 | Valid. Client proceeds. |
| 401, 403 | `error.InvalidToken`. CLI re-prompts for a new token. |
| >=400 (other) | `error.ControlPlaneError`. |

### Token-stdin login

`zigttp login --token-stdin` reads a token from stdin, calls `POST /v1/auth/token/verify`, and on success writes `~/.zigttp/credentials` directly. No server-side state change is required beyond whatever `/token/verify` does.

## Deploy session

A deploy session is a short-lived envelope that carries everything the client needs to build and push an OCI image and then ask the provider to run it. The client fetches it once per `zigttp deploy` invocation.

**`POST /v1/deploy/session`**

Header: `Authorization: Bearer <token>`.

Request body:

```json
{
  "projectName": "demo",
  "region": "us-central",
  "contract": { "routes": [], "egress": { "hosts": [] } },
  "contractSha256": "hex-sha256 of the canonicalized contract JSON"
}
```

`region` defaults to `"us-central"` on the client (`control_plane.zig:7`). The client now compiles first, serializes the handler contract to canonical JSON, hashes that canonical form, and sends both the contract object and `contractSha256` to the control plane.

Response 200:

```json
{
  "provider": "northflank",
  "registryHost": "registry.zigttp.dev",
  "namespace": "u-42",
  "registryUsername": "tok",
  "registryPassword": "pw",
  "scopeId": "proj-1",
  "planId": "plan-nf-basic",
  "providerApiToken": "nf-token",
  "registryCredentialId": "cred-1",
  "region": "us-east",
  "urlHint": "https://demo-u42.zigttp.app",
  "expiresAt": 1234567890
}
```

Required fields: `provider`, `registryHost`, `namespace`, `registryUsername`, `registryPassword`, `scopeId`, `planId`, `providerApiToken`, `region`. Missing any of them produces `error.InvalidSessionResponse`.

Optional fields: `registryCredentialId`, `urlHint`, `expiresAt` (unix seconds; null means no expiry advertised), `grantIds` (reusable capability grants that enabled this approval).

Response 202:

```json
{
  "status": "plan_required",
  "planId": "plan-1",
  "reviewUrl": "https://control.example.com/deploy/plans/plan-1",
  "baselineSha": "prev-sha",
  "proposedSha": "next-sha",
  "diff": {
    "reasons": ["new env read: SECRET_KEY"]
  },
  "grantCoverage": {
    "coveredReasons": ["new env read: DATABASE_URL"],
    "uncoveredReasons": ["new effect: write"]
  }
}
```

When the control plane returns `202`, the CLI does not build or upload anything. It prints the review URL plus a short summary of which risky additions are already covered by existing grants and which still need manual approval, then exits without proceeding.

Status codes: `401` and `403` are `error.NotSignedIn`; `202` is a structured review-needed response; anything else `>=400` is `error.ControlPlaneError`.

### Provider enum

Currently the only legal `provider` value is `"northflank"`. An unknown value produces `error.InvalidSessionResponse`. Adding a new provider requires extending `types.Provider.fromString` in `types.zig:3` and shipping a new adapter module alongside `northflank_adapter.zig`.

### Session refresh

When a session has `expiresAt` within `skew_seconds` of `now`, the client re-calls `POST /v1/deploy/session` with the exact same request body, including the contract and hash. If the refreshed session has a different `namespace` or `registryHost` than the original, the refresh is rejected with `error.SessionIdentityChanged` because the OCI image and registry ref were already derived from the old values. Self-hosted control planes must never rotate the user's namespace or registry identity mid-session.

## Build pipeline

Between fetching the session and pushing the image, the client runs a cross-compile locally:

```bash
zig build \
  -Dhandler=<resolved-handler-path> \
  -Doptimize=ReleaseFast \
  -Dtarget=<target-triple> \
  --prefix <project-root>/.zigttp/deploy-build/<target-triple>
```

`target-triple` comes from `types.Arch.targetTriple()`:

- `amd64` → `x86_64-linux-musl`
- `arm64` → `aarch64-linux-musl`

The built binary is read, SHA-256 digested, and packaged into an OCI image by the `deploy/oci/` subpackage. `build.zig` must live at the project root; the builder walks up from the handler path looking for it.

## Proven-fact OCI labels

The image config carries the handler's verified contract as OCI labels (source: `deploy.zig:411` onward). These labels are the payload of zigttp's differentiator - a runtime plane can introspect them to enforce policy, surface capabilities to dashboards, or reject images that regressed a proof.

### Boolean facts

| Label | Meaning |
|---|---|
| `zigttp.proof-level` | Enum: `none`, `extracted`, `proven` (or similar; `ProofLevel.toString()`). |
| `zigttp.retry-safe` | Handler is safe to retry end-to-end (no non-idempotent side effects). |
| `zigttp.read-only` | No virtual module calls with `.write` effect. |
| `zigttp.injection-safe` | No `user_input` labels reach SQL, HTML, or command sinks. |
| `zigttp.idempotent` | Safe for at-least-once delivery. |
| `zigttp.state-isolated` | No module-scope mutation from within the handler body. |
| `zigttp.results-safe` | All Result-producing calls are checked before `.value` access. |
| `zigttp.fault-covered` | Fault coverage pass detected no critical-severity unchecked failure paths. |
| `zigttp.env-proven` | All env-var keys are literal and enumerated in the contract. |
| `zigttp.egress-proven` | All outbound hosts are literal and enumerated. |
| `zigttp.cache-proven` | All cache namespaces are literal and enumerated. |

### Enumerated facts

These are JSON-array-valued labels, omitted when the list is empty:

| Label | Contents |
|---|---|
| `zigttp.env-vars` | Strings: `["PORT", "DB_URL", ...]` |
| `zigttp.egress-hosts` | Strings: `["api.example.com", ...]` |
| `zigttp.cache-namespaces` | Strings: `["sessions", "rates", ...]` |
| `zigttp.routes` | Strings: `["GET /health", "POST /links", ...]` |

### Scalar facts

| Label | Meaning |
|---|---|
| `zigttp.max-io-depth` | Integer. Upper bound on virtual-module calls per request, used to derive a Lambda timeout or similar capacity bound. |

Self-hosted runtimes can rely on these labels being stable across releases, but the boolean set may grow. Consumers should tolerate unknown `zigttp.*` labels.

## Service provisioning (Northflank)

After the image is pushed, the client calls the Northflank API directly using the `providerApiToken` from the session. This is the only provider currently wired in.

**`PUT https://api.northflank.com/v1/projects/{scopeId}/services/deployment`**

Header: `Authorization: Bearer {providerApiToken}`. Body: a Northflank deployment spec assembled from the handler's env vars, the image ref produced by the build, the `planId` from the session, and the `registryCredentialId` when present.

Successful responses: 200 or 201. 401 / 403 produces `error.ProviderUnauthorized`. Anything else produces `error.NorthflankDeployFailed`.

**`GET https://api.northflank.com/v1/projects/{scopeId}/services/{serviceId}`**

Polled every 2 seconds (`poll_interval_ms = 2000`) up to `default_deadline_seconds = 120` seconds when the user passes `--wait`. The client maps the provider's status string to one of `{pending, running, failed}`:

- `running`, `completed`, `healthy` → `.running`
- `failed`, `errored`, `crashloop` → `.failed`
- everything else → `.pending`

A self-hosted control plane that wants to replace Northflank must either (a) expose the same two endpoints under a Northflank-compatible contract, or (b) ship a new adapter module and have `types.Provider.fromString` recognize a new enum variant.

## Drift detection

The client keeps a state file under `.zigttp/state.json` (project-local) recording the most recently deployed image ref per service. Before issuing the PUT, it compares the computed image ref to the stored one. Mismatch plus the absence of `--confirm` produces `error.DriftDetected`, the CLI prints the diff to stderr, and exits with code `3`. `--confirm` acknowledges the drift and proceeds. Exit codes are documented in `docs/deploy-tutorial.md`:

- `0` - deploy completed
- `2` - build or upload failed
- `3` - drift detected without `--confirm`
- `4` - authentication missing or invalid

## Minimum viable self-hosted contract

To run a self-hosted control plane behind `ZIGTTP_CONTROL_PLANE_URL`, you must implement:

1. `POST /v1/auth/device/start`, `POST /v1/auth/device/poll`, `POST /v1/auth/token/verify` with the status code semantics above, or issue tokens out-of-band and document the token format.
2. `POST /v1/deploy/session` returning the full envelope with all required fields and a stable `namespace` + `registryHost` per user.
3. `GET /v1/deploy/plans/{id}` plus `POST /v1/deploy/plans/{id}/approve` and `POST /v1/deploy/plans/{id}/reject` for scripted review workflows. `approve` should accept `{"mode":"once"}` or `{"mode":"grant"}` and may also accept `{"expiresAt":"ISO-8601"}` when creating an expiring grant.
4. `GET /v1/grants`, `GET /v1/projects/{name}/grants`, and `POST /v1/grants/{id}/revoke` so the CLI can inspect and revoke reusable grants.
5. An OCI registry reachable at `registryHost` that accepts basic auth with `registryUsername` + `registryPassword` and stores images under `namespace/<service-name>`.
6. Either a Northflank-compatible provider API at `https://api.northflank.com/...` (unlikely) or a fork that adds a new adapter module.

The provider API is the hardest boundary to emulate. Until a non-Northflank adapter ships, self-hosting is effectively Northflank-only.

## Known gaps

- No rate limiting contract. Clients do not read `Retry-After`; servers must not depend on the client backing off.
- No API versioning beyond the `/v1/` prefix. Breaking changes would need a `/v2/` prefix and a coordinated client update.
- No pagination, no list endpoints. The client only pushes; it does not enumerate existing services.
- No documented logout endpoint. `zigttp logout` clears the local credentials file only.
