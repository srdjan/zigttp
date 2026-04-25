# zigttp + zigts Policy Wasm Capability Gating Specification

## Status

Version: 0.1  
Target systems: `zigttp`, `zigts`  
Primary goal: deterministic capability gating for runtime effects and compiler-adjacent operations.

---

## 1. Purpose

This specification defines a policy enforcement layer for `zigttp` and `zigts` using sandboxed policy artifacts, preferably WebAssembly components.

The policy layer exists to answer one question before any sensitive capability is used:

> Is this effect admissible in this context?

For `zigttp`, this applies to request handling, file access, outbound network access, database writes, runtime configuration, and long-running compute.

For `zigts`, this applies to compiler-hosted tools, expert-agent surfaces, code-generation helpers, internal mutation APIs, and any future self-hosted skill or tool interface.

---

## 2. Non-goals

This spec does not define:

- a general workflow engine
- an agent governor
- a user-facing authorization product
- a full identity verification flow
- a replacement for type checking in `zigts`

The policy layer complements static checks. It does not replace them.

---

## 3. Architectural Principle

`zigttp` and `zigts` should preserve the existing capability-injection style:

```zig
handler(req, caps)
```

Policy enforcement should be attached to capabilities, not scattered through application logic.

The preferred shape is:

```text
request / tool call
    → construct policy input
    → policy_check(input)
    → allow / deny
    → invoke gated capability only if allowed
```

---

## 4. Macrofx Extraction Note

`macrofx` should not be revived as a framework.

The useful idea to extract is narrow:

> declarative metadata describing an intended effect should resolve into runtime capability enforcement.

For `zigttp` and `zigts`, this means policy metadata can describe the action being attempted:

```ts
policy({ action: "db.write", resource: "users" })
```

At runtime, that metadata becomes a structured policy input. The old macrofx hook system is not needed. The extracted concept is simply:

```text
metadata → policy input → gated capability
```

This prevents policy calls from becoming ad hoc boilerplate.

---

## 5. Core Model

### 5.1 PolicyInput

Conceptual `zigts` definition:

```ts
type Actor =
  | { kind: "anonymous" }
  | { kind: "user"; id: string }
  | { kind: "service"; id: string }
  | { kind: "compiler_tool"; id: string }

type Resource = {
  kind: string
  id?: string
  attributes?: Record<string, unknown>
}

type PolicyInput = {
  actor: Actor
  action: string
  resource?: Resource
  argsHash?: string
  env: {
    service: "zigttp" | "zigts"
    mode: "dev" | "test" | "prod"
  }
  timestamp: number
}
```

### 5.2 PolicyResult

```ts
type PolicyResult =
  | { kind: "allow" }
  | { kind: "deny"; reason: string }
```

For `zigttp` and `zigts`, the result should initially stay binary: allow or deny.

Avoid `modify` or `step_up` in this layer. Those belong in higher-level systems such as Metadoor.

---

## 6. Wasm Component Interface

The policy engine should be exposed through a narrow ABI.

Example WIT:

```wit
package zigttp:policy

type actor = variant {
  anonymous,
  user(string),
  service(string),
  compiler-tool(string),
}

type resource = record {
  kind: string,
  id: option<string>,
}

type environment = record {
  service: string,
  mode: string,
}

type input = record {
  actor: actor,
  action: string,
  resource: option<resource>,
  args-hash: option<string>,
  env: environment,
  timestamp: u64,
}

type result = variant {
  allow,
  deny(string),
}

interface policy {
  policy-check: func(input: input) -> result
}
```

---

## 7. zigttp Integration

### 7.1 Enforcement Placement

```text
router
  → middleware
  → handler(req, caps)
  → gated capability
  → policy_check
  → real capability
```

The handler should receive already-gated capabilities where possible.

### 7.2 Capability Wrapper Pattern

```zig
const GatedDb = struct {
    inner: Db,
    policy: Policy,

    pub fn write(self: *GatedDb, ctx: RequestContext, data: Data) !void {
        const decision = try self.policy.check(.{
            .actor = ctx.actor,
            .action = "db.write",
            .resource = .{ .kind = "db", .id = "primary" },
        });

        switch (decision) {
            .allow => return self.inner.write(data),
            .deny => return error.Forbidden,
        }
    }
};
```

### 7.3 Capabilities to Gate

Initial capability classes:

| Capability | Example action |
|---|---|
| Database write | `db.write` |
| Database migration | `db.migrate` |
| File read | `fs.read` |
| File write | `fs.write` |
| Outbound HTTP | `http.outbound` |
| Runtime config mutation | `runtime.config.write` |
| Long-running compute | `compute.long` |

### 7.4 Fast Path

Not every operation should call Wasm.

Use static allowlists for trivial safe operations:

```zig
if (isStaticSafeRead(req)) {
    return next(req);
}

return policyChecked(req);
```

---

## 8. zigts Integration

### 8.1 Purpose

`zigts` should use the same policy mechanism for compiler-hosted capabilities and tool interfaces.

Primary targets:

- expert-code-generation tools
- compiler introspection APIs
- repository mutation tools
- build execution
- filesystem projections
- embedded knowledge or skill tools

### 8.2 Example Actions

| Action | Meaning |
|---|---|
| `zigts.compile` | Compile user-provided source |
| `zigts.inspect_ast` | Expose AST or IR inspection |
| `zigts.generate_code` | Generate code through hosted expert tool |
| `zigts.modify_compiler` | Attempt to mutate compiler internals |
| `zigts.run_build` | Execute build command |
| `zigts.skill.query` | Query embedded skill/knowledge base |

### 8.3 Recommended Rule

User-facing agents may author `zigts` code, but must not mutate the compiler unless explicitly running in compiler-author mode.

Example policy intent:

```rego
allow if {
  input.action == "zigts.generate_code"
}

allow if {
  input.action == "zigts.compile"
}

deny_reason := "compiler mutation is not allowed from user-facing mode" if {
  input.action == "zigts.modify_compiler"
  input.env.mode != "compiler_author"
}
```

---

## 9. Policy Authoring

Recommended first implementation:

```text
Rego → OPA Wasm → Wasm component wrapper
```

Policy code should be declarative and side-effect free.

Rules should avoid:

- network access
- filesystem access
- random values
- internal clocks
- mutable global state

All contextual values should arrive through `PolicyInput`.

---

## 10. Performance Requirements

| Concern | Requirement |
|---|---|
| Wasm instantiation | Never instantiate per request |
| Policy latency | Target under 50 microseconds for hot-path checks |
| Allocation behavior | Avoid allocations in hot path |
| Timeout | Strict upper bound per policy call |
| Failure mode | Fail closed |

---

## 11. Failure Handling

| Failure | Required behavior |
|---|---|
| Wasm trap | Deny |
| Policy timeout | Deny |
| Malformed input | Deny |
| Unknown action | Deny by default |
| Missing resource | Deny unless action explicitly allows missing resource |

---

## 12. Observability

Every denial should emit a structured event:

```json
{
  "event": "policy_denied",
  "service": "zigttp",
  "action": "db.write",
  "resource": { "kind": "db", "id": "primary" },
  "reason": "actor_not_allowed"
}
```

For performance reasons, allowed decisions may be sampled.

Denials should not be sampled.

---

## 13. Testing

Policy tests should use golden fixtures:

```text
fixtures/
  allow_db_write_admin.json
  deny_db_write_anonymous.json
  deny_compiler_mutation_user_mode.json
```

Each fixture contains:

```json
{
  "input": {},
  "expected": { "kind": "deny", "reason": "..." }
}
```

Required test types:

- golden allow/deny tests
- malformed input tests
- unknown action tests
- timeout tests
- Wasm trap tests
- policy version rollback tests

---

## 14. Policy Lifecycle

```text
policy source
  → build
  → test
  → sign/checksum
  → package
  → deploy
  → observe
  → rollback if needed
```

Artifacts should be versioned and checksum-pinned.

A runtime should be able to run with a pinned policy version.

---

## 15. Recommended Implementation Phases

### Phase 1

- Define `PolicyInput` and `PolicyResult`
- Implement local in-process mock policy checker
- Add gated wrappers for database and outbound HTTP

### Phase 2

- Add Rego policy
- Compile to Wasm
- Load policy through a pooled runtime
- Add golden tests

### Phase 3

- Integrate `zigts` hosted tool gating
- Add compiler-mode policy distinctions
- Add structured denial observability

### Phase 4

- Add component-model boundary
- Add signing/checksum verification
- Add policy rollback command

---

## 16. Main Risks

### 16.1 Over-centralization

Risk: every operation calls policy, causing latency creep.

Mitigation:

- use static fast paths
- cache decisions for pure, repeated checks
- batch checks when safe

### 16.2 Policy sprawl

Risk: policy rules become unreadable.

Mitigation:

- keep action names stable
- group policy by capability class
- reject rules that inspect huge argument blobs
- pass `argsHash` instead of raw arguments when possible

### 16.3 Debugging opacity

Risk: denied operations become difficult to explain.

Mitigation:

- require structured denial reasons
- store policy version with every denial
- keep golden tests human-readable

---

## 17. Summary

For `zigttp`, policy Wasm turns runtime effects into gated capabilities.

For `zigts`, the same mechanism protects compiler-hosted tools and prevents accidental or unauthorized mutation of compiler internals.

The shared philosophy is:

```text
static correctness from zigts
runtime admissibility from policy
```
