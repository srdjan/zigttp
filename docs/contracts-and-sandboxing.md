# Contracts, Auto-Sandboxing, and Evolution

The compiler extracts a contract from every handler. The contract
describes what the handler does before it runs and is used to derive
the runtime sandbox, compare versions across deployments, and emit
public artifacts (OpenAPI, TypeScript SDK).

## Contract Manifest (`-Dcontract`)

Every precompilation extracts a contract from the handler's IR. Add
`-Dcontract` to also emit it as `contract.json`. The compiler extracts:

- **Virtual modules** imported and which functions are used
- **Environment variables** accessed via `env("NAME")`; literal names
  are enumerated, dynamic access is flagged
- **Outbound hosts** called via `fetchSync("https://...")`; hosts
  extracted from URL literals
- **Internal service calls** made via `serviceCall("name", "METHOD
  /path", init)`; service names, route signatures, and statically
  proven params/query/header/body keys are captured
- **System-linked payload facts** for named internal edges; target
  response statuses, JSON payload proof, and payload-proof gaps
- **Cache namespaces** used by `cacheGet`/`cacheSet`/etc.
- **SQL queries** registered with `sql("name", "...")`; names,
  statement kinds, and touched tables after schema validation
- **Scope usage** from `scope("name", fn)`; literal scope names,
  whether scope callbacks stay dynamic, the maximum nested scope depth
- **API surface** from proven routes; method/path, path/query/header
  params, JSON request bodies, response variants, bearer auth metadata
- **Handler properties** derived from the internal effect summary of
  virtual module calls, cache reads, scope usage, egress, and
  nondeterministic builtins (`pure`, `read_only`, `stateless`,
  `retry_safe`, `deterministic`)
- **Behavioral paths**: every execution path through the handler with
  route, branching conditions, I/O sequence, and response status
  (exhaustive when the path count stays below 1024)
- **Verification results** when combined with `-Dverify`
- **Route patterns** when combined with `-Daot`

```json
{
  "version": 12,
  "modules": ["zigttp:auth", "zigttp:cache", "zigttp:scope"],
  "functions": {
    "zigttp:auth": ["jwtVerify", "parseBearer"],
    "zigttp:cache": ["cacheGet", "cacheSet"],
    "zigttp:scope": ["scope", "ensure"]
  },
  "env": { "literal": ["JWT_SECRET"], "dynamic": false },
  "egress": { "hosts": ["api.example.com"], "dynamic": false },
  "cache": { "namespaces": ["sessions"], "dynamic": false },
  "scope": {
    "used": true, "names": ["request", "enrich-user"],
    "dynamic": false, "maxDepth": 2
  },
  "sql": {
    "backend": "sqlite",
    "queries": [
      { "name": "listTodos", "operation": "select", "tables": ["todos"] }
    ],
    "dynamic": false
  },
  "properties": {
    "pure": false, "readOnly": false, "stateless": false,
    "retrySafe": false, "deterministic": true, "hasEgress": true
  },
  "behaviors": [
    {
      "method": "GET", "pattern": "/users/:id", "status": 200,
      "ioDepth": 2, "failurePath": false,
      "conditions": [
        {"kind": "io_ok", "module": "auth", "func": "jwtVerify"},
        {"kind": "io_ok", "module": "cache", "func": "cacheGet"}
      ],
      "ioSequence": [
        {"module": "auth", "func": "jwtVerify"},
        {"module": "cache", "func": "cacheGet"}
      ]
    },
    {
      "method": "GET", "pattern": "/users/:id", "status": 401,
      "ioDepth": 1, "failurePath": true,
      "conditions": [
        {"kind": "io_fail", "module": "auth", "func": "jwtVerify"}
      ],
      "ioSequence": [
        {"module": "auth", "func": "jwtVerify"}
      ]
    }
  ],
  "behaviorsExhaustive": true
}
```

The `"dynamic": false` fields are the key signal. They mean "the
compiler enumerated every value statically." When a handler uses a
variable instead of a string literal (`env(someVar)` instead of
`env("JWT_SECRET")`), the contract honestly reports `"dynamic": true`.

## Auto-Sandboxing

The contract is used to derive a `RuntimePolicy` embedded in the
binary. Sections with `dynamic: false` are restricted to exactly the
proven literals. Sections with `dynamic: true` remain unrestricted.

```text
Sandbox: complete (all access statically proven)
  env: restricted to [JWT_SECRET] (1 proven, no dynamic access)
  egress: restricted to [api.example.com] (1 proven, no dynamic access)
  cache: restricted to [sessions] (1 proven, no dynamic access)
  sql: restricted to [listTodos] (1 proven, no dynamic access)
Handler Properties:
  ---    pure            handler is a deterministic function of the request
  ---    read_only       no state mutations via virtual modules
  ---    stateless       independent of mutable state
  ---    retry_safe      disabled when scope-managed cleanup or bare writes are present
  PROVEN deterministic   no Date.now() or Math.random()
```

Self-extracting binaries parse the embedded contract at startup:
proven env vars are validated (missing vars fail fast instead of
causing a 500 on first request), proven routes reject non-matching
requests at the HTTP layer before entering JS, and proven handler
properties are logged for operator visibility.

Handlers proven `deterministic` and `read_only` that also read no
request headers or body have their GET/HEAD responses cached at
runtime and served from Zig memory without entering JS. The cache key
is method+URL only, so a handler whose response depends on a request
header (auth, content negotiation) is excluded. The
`X-Zigttp-Proof-Cache: hit` response header confirms a cache hit.

### When each contract assertion is enforced

The contract is consulted at four distinct moments. Knowing which
moment owns which assertion is the difference between "the binary
won't boot" and "individual requests get rejected".

**Build time** (before the binary is produced):
- Type and effect checks against `Spec<...>` and `Effects<...>` on the
  handler's return type. Failures stop compilation; no binary is
  produced.
- Explicit policy override validation against the contract when
  `-Dpolicy` is passed. A policy that admits capabilities the contract
  did not prove is rejected.
- Replay regressions against `-Dreplay=traces.jsonl`.
- Upgrade verdict against `-Dprove=old:traces` (when classified
  `breaking`, the build fails unless `--force-swap` is on the eventual
  runtime invocation).

**Process startup** (once, when the self-extracting binary boots):
- `embedded_handler.capability_policy` is in scope. The precompile
  pipeline generates this as a comptime `pub const RuntimePolicy` in
  the embedded handler module; the runtime applies it to each
  context via `runtime_config.zig:applyEmbeddedCapabilityPolicy`.
  Because the value is comptime-baked at build time, **the
  RuntimePolicy is fixed for the lifetime of the binary** — there is
  no contract-file parse at boot that could change it.
- `proven_env` literals are validated against the process environment
  (`proof_adapter.zig`). A missing required var fails the process
  rather than 500-ing the first request.
- Pooling policy is derived from contract properties
  (`contract_runtime.zig:derivePoolingPolicy`) and applied to the
  HandlerPool exactly once via `pool.setPoolingPolicy(...)` from
  `server.zig:start`. In `zigttp dev --watch --prove`, each accepted
  swap re-derives the pooling policy from the new contract before the
  handler pool is reused.
- Attestation envelope (`Zigttp-Attest`) is materialized for
  `GET /.well-known/zigttp-attest`.

**Per request** (in the dispatch path, before any JS runs):
- Route pre-filter: when `routes_dynamic = false`, the proven route
  table in `contract_runtime.zig` rejects non-matching method/path
  combinations at the HTTP layer with a 404, never acquiring a
  runtime. When `routes_dynamic = true`, every request falls through
  to the handler and route matching is the handler's responsibility
  — the contract surface stops being a runtime gate for routing.
- Proof cache lookup: handlers proven `deterministic` + `read_only`
  that read no request headers or body serve `GET`/`HEAD` from the
  Zig-side cache without entering JS (`X-Zigttp-Proof-Cache: hit`).
  Header/body-dependent handlers are excluded because the cache key is
  method+URL only.
- Per-name policy checks for SDK-facing categories (env, cache, sql,
  sql-write): the runtime exposes `allows{Env,CacheNamespace,SqlQuery,
  SqlWrite}ForActiveModule` in `module_binding.zig`. Each consults
  `ctx.capability_policy.allows*(name)` and rejects the call when the
  name is outside the allowlist. The corresponding SDK module bindings
  must declare `.policy_check` in `required_capabilities` — otherwise
  the call panics with an undeclared-capability error. These two
  layers are independent: `required_capabilities` is the module's
  binding-level declaration of which gates it consults;
  `ctx.capability_policy` is the contract-derived allow/deny data
  those gates consult.
- Outbound egress: handled inside the runtime's `fetch` path
  (`zruntime.zig:outboundHostViolation`) via the unwrapped
  `ctx.capability_policy.allowsEgressHost(host)`. The check sits in
  the shared `parseFetchArgs`, so it covers both the sequential and
  the `zigttp:io` parallel/race fetch paths. This is a runtime-
  initiated check on the URL host, not an SDK module call, so it
  does not go through the `*ForActiveModule` wrappers and does not
  require any binding to declare `.policy_check`. The allowlist
  matches on host only: it does not restrict the port, so an allowed
  host permits any port on that host.

**Hot swap** (`--watch --prove`):
- Re-runs the build-time contract diff against the running version
  before applying the swap. A `breaking` verdict blocks the swap
  unless `--force-swap` is in effect.
- `runtime_pool.zig:reloadHandler` updates `handler_code` and
  `handler_filename`, clears the bytecode cache, and invalidates
  every idle runtime so it recompiles from the new source on next
  acquire. In-flight requests finish on the old bytecode and are
  recycled into the new code on their next cycle.
- `server.zig:updateContract` (called by `live_reload`) replaces the
  proven-routes table and the proof cache so per-request route
  gating and the read-only cache reflect the new contract.
- In the interpreted `dev`/`serve` path the embedded `RuntimePolicy`
  is the empty stub, so `live_reload` derives the full runtime policy
  from each accepted contract and applies it via
  `server.zig:setDevCapabilityPolicy` →
  `runtime_pool.zig:setDevCapabilityPolicy` (plumbed as
  `RuntimeConfig.dev_capability_policy`). That invalidates idle
  runtimes so env, egress, cache, and sql gates re-create from the new
  contract. If policy allocation fails, the staged policy fails closed.
- `server.zig:updateContract` also refreshes `pool.pooling_policy`
  from the new contract unless an explicit lifecycle override was
  configured. In-flight requests finish on the old runtime generation;
  later acquisitions use the re-derived lifecycle policy.
- In a precompiled or deployed binary the policy is the comptime
  constant baked at build time and is never swapped; tightening or
  widening it requires a rebuild.
- Durable handlers refuse hot swap entirely because replay state
  depends on handler identity.

A narrow gap to be aware of: a handler that flips `routes_dynamic`
from `false` to `true` across a hot swap loses the route pre-filter
on the next request. This is intentional — the new contract honestly
says routing is no longer statically enumerated — but operators
monitoring route-shape changes should treat that transition as a
deliberate widening of the request surface.

## Explicit Policy Override (`-Dpolicy`)

To override auto-derived sandboxing with a stricter or different
policy, pass an explicit policy file. The policy is validated against
the contract at build time and enforced at runtime. Local file-import
handlers are covered: capability usage is aggregated across the module
graph before validation.

```json
{
  "env":    { "allow": ["JWT_SECRET"] },
  "egress": { "allow_hosts": ["api.example.com"] },
  "cache":  { "allow_namespaces": ["sessions"] },
  "sql":    { "allow_queries": ["listTodos"] }
}
```

Omit a section to leave that capability unrestricted. If a section is
present, dynamic access in that category is rejected because zigttp
cannot fully enumerate it.

## OpenAPI and TypeScript SDK (`-Dopenapi`, `-Dsdk=ts`)

The same proven route facts can be emitted as OpenAPI and as a
generated TypeScript client:

```bash
zig build -Dhandler=examples/routing/api-surface.ts -Dcontract -Dopenapi -Dsdk=ts
```

This writes three sibling artifacts in `src/generated/`:

- `contract.json`
- `openapi.json`
- `client.ts`

The current API emitters include facts the compiler can prove without
guessing:

- route method and path
- path, query, and header params reached through literal access
- proven JSON request bodies from `validateJson(...)`, `coerceJson(...)`,
  and `decodeJson(...)`
- proven form request bodies from `decodeForm(...)`
- typed query params from `decodeQuery(...)`
- proven response variants, including multiple status codes when
  statically visible
- bearer auth metadata
- `x-zigttp-*` hints whenever part of the surface stays dynamic

The generated SDK only exposes typed helpers for routes it can prove
end to end. Everything else remains available through `requestRaw()`
and is listed in `skippedOperations`.

```ts
import { createClient } from "./src/generated/client";

const api = createClient({ baseUrl: "https://api.example.com" });

const result = await api.postProfilesId({
    params: { id: "user_123" },
    query: { verbose: true },
    body: { displayName: "Ada" },
    headers: { "x-client-id": "cli-42" },
});
```

## Deterministic Replay (`--trace`, `--replay`, `-Dreplay`)

The restricted JS subset (no async, no exceptions, no side-effecting
builtins) makes handlers deterministic pure functions of their request
and virtual module responses. The replay system exploits this.

**Record** traces during normal operation:

```bash
zigttp serve handler.ts --trace traces.jsonl
```

Every virtual module call, `fetchSync` response, `Date.now()`
timestamp, and `Math.random()` value is recorded alongside the request
and response.

**Replay** traces against a modified handler to detect regressions:

```bash
zigttp serve --replay traces.jsonl handler-v2.ts
```

Reports identical, status-changed, and body-changed results with
structured diffs.

**Build-time replay** fails the build if regressions are detected:

```bash
zig build -Dhandler=handler-v2.ts -Dreplay=traces.jsonl
```

## Proven Evolution (`-Dprove`)

Compare two handler versions and classify the upgrade:

```bash
zig build -Dhandler=handler-v2.ts -Dprove=old-contract.json:traces.jsonl
```

Two diff levels run against the old and new contracts. The surface
diff compares I/O capabilities. The behavioral diff compares every
execution path, matching by route and branching conditions, then
classifying each as preserved, response-changed, removed, or added.

Property regressions carry severity. Losing `retry_safe` or
`injection_safe` is critical. Losing `deterministic` or `idempotent`
is a warning. Losing `pure` is informational.

The upgrade verdict combines these signals:

- **safe**: identical behavior, no property regressions
- **safe_with_additions**: new paths or capabilities added, existing
  behavior preserved
- **breaking**: paths removed, responses changed, or critical property
  lost
- **needs_review**: structurally OK but warning-level regressions or
  significant coverage gaps

Output: `proof.json` (machine-readable certificate),
`proof-report.txt` (human-readable), and `upgrade-manifest.json`
(verdict with full breakdown).

The standalone `zigts prove old.json new.json` CLI compares contracts
without rebuilding (exit 0 for safe, 1 for breaking, 2 for
needs_review).

## Proven Live Reload (`--watch --prove`)

`zigttp dev --watch --prove` watches handler files and hot-swaps them
in-process on every save. The server recompiles the handler, extracts
its behavioral contract, diffs it against the running version, and
applies the change only when the upgrade verdict is `safe` or
`safe_with_additions`. Breaking changes block the swap and print the
diff. `--force-swap` overrides the block.

Without `--prove`, `--watch` hot-reloads without contract proof.
Compilation errors keep the old handler running. Durable handlers
refuse live swap because replay state depends on handler identity.

## Author-declared specs (`Spec<...>`)

Declare which compiler-proven properties your handler must satisfy
directly in the return type:

```typescript
import type { Spec } from "zigttp:types";

type Guardrails = Spec<"idempotent" | "deterministic" | "no_secret_leakage">;

function handler(req: Request): Response & Guardrails {
    // ...
}
```

The verifier discharges each spec against the inferred
`HandlerProperties`; failures emit `ZTS500` with a per-property
suggestion, `ZTS501` for spec-vs-import contradictions, and `ZTS502`
for unknown names.

Helpers carry the companion `Proof<T, "...">` capsule: the compiler
discharges `total`, `pure`, `read_only`, and `deterministic` against
each helper's own body, so a handler's proof composes across call
boundaries instead of dying at the first helper (`ZTS606` if an
unproven effectful helper breaks a demanded property).

The capability dual, `Effects<T, "...">`, declares a least-privilege
ceiling on a function's effect row (checked `inferred ⊆ declared`) and
on the handler's return type becomes a budget that bounds every
reachable helper (`ZTS503-ZTS508`, `ZTS607`).

## Property ratchet

Every `contract.json` carries a top-level `provenSpecs` array (the
canonical, ordered list of properties the compiler proves true)
alongside the existing `properties` object, plus a `declaredSpecs`
array containing the effective active spec set. If the handler has no
`Spec<...>`, this array contains every supported v1 spec; an explicit
`Spec<...>` narrows it to the named specs. Both ride inside the signed
JWS payload, so a third party can diff two builds and see exactly which
property moved.

```bash
zigttp ratchet show <handler.ts>      # current proven set
zigttp ratchet check <handler.ts>     # diff active specs against proven; exit 1 if any unmet
```

Handlers that declare no `Spec<...>` ratchet against the default full
supported set.

## Behavioral contract

`-Dcontract` enumerates every execution path through the handler and
embeds them in `contract.json` as structured `behaviors`. Each path
records the route, branching conditions (which I/O calls succeed or
fail), the I/O sequence, and the resulting HTTP status.

The restricted JS subset has no back-edges and no exceptions, so path
enumeration is finite and exhaustive. Comparing two behavioral
contracts shows which paths were preserved, which changed response
codes, which were removed, and which are new.

## External enrichment flags

Five optional build flags accept external JSON files for
cross-referencing against compiler-proven contracts. These work with
any code generator or hand-written files; no specific tooling is
required.

- `-Dmanifest=<path>`: cross-references a declared manifest (routes,
  SQL tables, env vars) against the handler contract. Errors on
  declared items missing from code, warns on undeclared items found in
  code. Emits `manifest-alignment.json`.
- `-Dexpect-properties=<path>`: verifies handler-derived properties
  (`state_isolated`, `injection_safe`, `read_only`, etc.) match
  external expectations. Build fails on mismatches.
- `-Ddata-labels=<path>`: merges externally declared data-sensitivity
  labels (`secret`, `credential`, etc.) with the flow checker's
  heuristic labels. Violations of declared labels become build errors.
- `-Dfault-severity=<path>`: overrides fault severity classification at
  the route level. A route declared "critical" elevates all failable
  calls within it to critical severity for fault coverage diagnostics.
- `-Dreport=json`: emits a structured JSON build report aggregating
  verification, properties, fault coverage, flow analysis, manifest
  alignment, and property expectations into `report.json`.

All flags are optional and additive. Without them, nothing changes.
