# Zigttp Frontier

This document describes where zigttp is already differentiated, what should be
protected as its core identity, and which features are the strongest next bets
if the goal is to build the most distinctive TypeScript runtime possible.

## Core Thesis

zigttp should not compete as a general-purpose JavaScript runtime with a few
nice static checks layered on top. Its strongest shape is narrower and more
coherent:

- the execution model is compiler-visible
- effects are explicit imports, not ambient globals
- contracts are extracted from handlers as first-class artifacts
- deploy and rollout decisions consume those contracts
- agents operate with the compiler as the authority

That makes zigttp closer to a proof-aware TypeScript execution platform than a
faster Node or Deno clone.

## What Already Feels Unique

The current moat comes from the interaction between several subsystems, not from
one isolated trick:

- A pure-Zig runtime stack: parser, VM, GC, JIT, object model, handler pool.
- A restricted TypeScript subset chosen so compile-time reasoning stays
  structural.
- Compile-time verification of returns, unchecked `Result` use, unchecked
  optionals, state isolation, and fault coverage.
- Sound-mode type-directed analysis across truthiness, arithmetic, comparisons,
  and specialized code generation.
- Virtual modules as the effect boundary for auth, validation, caching, SQL,
  durable execution, service calls, logging, and structured concurrency.
- Contract extraction that feeds runtime sandboxing, route pre-filtering, proof
  cache, deploy manifests, and review flows.
- Straight-line durable execution and concurrent I/O without async/await,
  Promises, or an event loop.
- Compiler-aware agent tooling that treats the compiler and module metadata as
  the source of truth.

If zigttp keeps strengthening these connections, it remains hard to confuse
with mainstream runtimes.

## Near-Term Frontier

These are the highest-leverage features to build next.

### 1. Proofable Third-Party Virtual Modules

The extension model is the clearest next step. Third-party modules should be
able to participate in the same proof pipeline as built-ins by shipping
manifest-first metadata for:

- exports
- capabilities
- effect classes
- return kinds
- contract extraction rules
- trace mode
- state model

The compiler should reason over that manifest, and the runtime should enforce
it. The proof metadata is the real extension contract.

### 2. Compiler-Enforced Capability Fencing

Built-in virtual modules should only reach world-facing behavior through a
small, audited helper surface. No raw `Context`, no direct stdlib effect
escapes, no hidden access paths. That would turn capability declarations from
governance metadata into something the implementation must structurally obey.

### 3. Full Built-In Module Spec Governance

Built-in module specs should become the canonical authoring and review surface
for effects, laws, extraction rules, and required capabilities. The Zig binding
stays executable, but semantic review starts from the spec artifact.

### 4. First-Class Trace and Replay Workflow

Trace and replay should become a visible product surface, not just an internal
debugging aid. The interesting direction is to combine:

- recorded traces
- replay against new handler versions
- contract diffs
- proof and rollout explanations

That gives zigttp a debugging and regression model most runtimes do not have.

### 5. Proof-Native Upgrade Safety

Live reload and deploy review already consume contracts. The next step is to
make upgrade safety more legible: explain which routes, properties, effects, or
behavioral paths changed and which changes are compatible, additive, or risky.

## Mid-Term Frontier

Once the near-term foundation is in place, the next layer should extend proof
from single handlers to systems.

### Whole-System Contract Proofs

System linking should grow into a multi-service proof surface:

- which services call which downstream routes
- where auth is established and preserved
- where data labels and env dependencies cross boundaries
- which deploys widen system-level capability exposure

### Data-Plane Guarantees

The SQL, validation, and SDK-generation pieces should converge into one story:

- query allowlist proofs
- schema compatibility checks
- request and response contracts flowing into generated SDKs
- deploy gating for data-access expansion

### Proof-Directed Compilation

Proven properties should increasingly influence packaging and execution:

- stronger AOT and partial evaluation for pure or deterministic handlers
- more aggressive stripping of unused runtime surfaces
- specialization of virtual module calls when properties are already proven

### Durable Plain-Code Workflows

Durable execution should become richer without splitting into a second language
model. The winning constraint is simple: keep workflows as straight-line,
analyzable TypeScript.

## Moonshots

These are the category-defining ideas if the core lands well.

### Capability-Native Package Ecosystem

An ecosystem where packages declare capability needs, proof metadata, and
execution backends instead of inheriting ambient power from a generic runtime.

### Agent-Safe Runtime Authoring

A workflow where agents can safely author handlers and virtual modules because
the environment is constrained by compiler-visible capabilities, explicit specs,
and replayable contracts.

### Proof-Carrying Deploy Artifacts

OCI images and deployment manifests that carry machine-readable proofs,
compatibility classes, and effect deltas as first-class deployment data.

## Things To Avoid

The easiest ways to dilute the project's identity are:

- chasing broad browser or Node compatibility at the expense of analyzability
- expanding the language subset until the proof model becomes ad hoc
- exposing extension power before the manifest and capability model is strong
- marketing the project primarily as a benchmark story instead of a proof story
- letting AI tooling bypass compiler or module-spec authority

## Strategic Order

If the roadmap needs one crisp sequence, it should be:

1. finish built-in module spec governance and capability discipline
2. make third-party extensions proofable and manifest-first
3. make trace, replay, and upgrade-proof workflows product-grade
4. extend contracts from handlers to whole systems
5. use proven properties to drive compilation and deployment more aggressively

That path preserves the project's strongest idea: TypeScript execution where
effects, proofs, and operations all live in the same model.
