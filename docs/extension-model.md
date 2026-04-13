# zigttp Extension Model

This document defines a better extension model for zigttp virtual modules. It is a design note, not an implementation report.

The goal is simple: third-party modules should be easy to write, easy to distribute, and fully visible to the zigts proof system. The current extension path does not meet that bar yet. It is tied to Zig, tied to the current VM value ABI, and only partially integrated into the compiler analyses that make virtual modules valuable.

## Goals

- Let third-party modules participate in the same proof pipeline as built-in modules.
- Keep the extension contract stable even if the JS runtime internals change.
- Make package distribution possible without compile-time linking every module into zigttp.
- Preserve trace, replay, contract extraction, effect classification, and flow proofs.
- Keep the model small enough to reason about.

## Non-Goals

- Expose every runtime hook as an extension surface on day one.
- Let extensions bypass proof metadata or capability declarations.
- Freeze the internal implementation of the runtime or VM.
- Support arbitrary foreign code at the expense of determinism or replay.

## Current Problems

### 1. The SDK leaks the VM ABI

The current `zigttp-sdk` exposes a packed `JSValue` whose layout matches the runtime's NaN-boxed value representation. That works for a Zig-only demo, but it is not a good long-term extension boundary.

If the VM value layout changes, the extension ABI changes with it. That is backwards. The module contract should survive changes to `value.zig`.

### 2. The public extension contract lags the real compiler contract

`ModuleBinding` inside zigts already carries the facts the compiler needs:

- effect class
- return kind
- parameter kinds
- traceability
- contract extraction rules
- contract flags
- return labels
- failure severity
- required capabilities
- state model flags

The SDK mirrors only part of this surface. When the internal metadata evolves, the public extension path can drift.

### 3. Proof passes still treat extensions as second-class

Some analyses still scan only `zigttp:*` imports and skip `zigttp-ext:*`. That creates blind spots in:

- flow analysis
- sound mode / boolean checker import typing
- path generation

An extension model is not finished until every proof pass treats a registered extension the same way it treats an in-tree module.

### 4. Function names are globally unique

The current registry rejects duplicate export names across different modules. That is manageable for a small built-in set. It does not scale to an ecosystem.

Module lookup should key on `(specifier, export_name)`, not on export name alone.

### 5. Extensions are still compile-time linked

The current demo path appends extension bindings to a generated registry at build time. That is useful for testing, but it is not a package model.

## Design Principle

The proof metadata is the real extension contract. The execution backend is an implementation detail.

That single sentence should drive the redesign.

A module package should declare:

- what it exports
- what each export means to the compiler
- what host capabilities it consumes
- how its calls are traced and replayed
- what state model it needs

The compiler should trust that declaration enough to type-check and analyze handler code. The runtime should enforce that declaration enough to keep the analysis honest.

## Split The System In Two

The next model should separate two concerns that are mixed together today.

### 1. Manifest Layer

The manifest is the source of truth for the compiler.

It declares:

- module specifier
- exports
- parameter kinds
- return kind
- effect class
- failure severity
- return labels
- contract extraction rules
- contract flags
- required capabilities
- trace mode
- state model
- backend kind

### 2. Execution Layer

The execution layer implements the exports declared in the manifest.

Possible backends:

- `native-zig`
- `wasm-component`

The compiler should not care which backend is used once the manifest is loaded and validated.

## Two Classes Of Virtual Modules

Not every built-in virtual module should become an extension target.

### Proofable Modules

These fit the extension model well because their behavior can be described as pure or effectful calls over stable data:

- `zigttp:crypto`
- `zigttp:text`
- `zigttp:time`
- `zigttp:id`
- parts of `zigttp:validate`
- parts of `zigttp:cache`
- parts of `zigttp:sql`

### Runtime Intrinsics

These are tightly coupled to the runtime and should stay core until there is a clean resource model:

- `zigttp:router`
- `zigttp:io`
- `zigttp:durable`
- `zigttp:service`
- scope-style control-flow helpers

They depend on function values, host-owned callbacks, JS objects with runtime identity, or durable execution semantics that are deeper than a plain module call.

The extension model should start with proofable modules. That keeps the contract small and the guarantees strong.

## Package Format

Each module package should ship a manifest and one executable artifact.

```text
my-module/
  zigttp-module.json
  module.wasm
```

For a native Zig backend, the artifact could be a platform library or a statically linked package. The manifest format stays the same.

## Manifest Schema

This is a sketch, not a final schema.

```json
{
  "schema_version": 1,
  "specifier": "zigttp-ext:redis",
  "backend": "wasm-component",
  "state_model": "instance",
  "required_capabilities": ["network", "clock"],
  "exports": [
    {
      "name": "get",
      "params": ["string", "string"],
      "returns": "optional_string",
      "effect": "read",
      "trace_mode": "auto",
      "failure_severity": "expected",
      "return_labels": ["external"],
      "contract_extractions": [
        { "arg_position": 0, "category": "cache_namespace" }
      ],
      "contract_flags": []
    },
    {
      "name": "set",
      "params": ["string", "string", "string", "number"],
      "returns": "boolean",
      "effect": "write",
      "trace_mode": "auto",
      "failure_severity": "none",
      "return_labels": [],
      "contract_extractions": [
        { "arg_position": 0, "category": "cache_namespace" }
      ],
      "contract_flags": []
    }
  ]
}
```

The runtime should reject manifests that declare unsupported categories, labels, or capabilities.

## Canonical Value Model

The extension ABI should not expose raw VM values.

Use a closed value model that maps cleanly to the compiler's proof categories and to multiple backends:

- `bool`
- `s64`
- `f64`
- `string`
- `bytes`
- `list<value>`
- `record<string, value>`
- `option<T>`
- `result<T, E>`
- `unit`

This model is more restrictive than full JS. That is a feature, not a bug.

It keeps the extension surface:

- portable across backends
- serializable for trace and replay
- understandable to the proof system
- independent of hidden classes and VM object identity

## Extension-Safe Type Subset

Third-party modules should start with a strict type subset.

Allowed:

- scalar values
- records with string keys
- lists
- option and result wrappers
- byte buffers

Not allowed in the first version:

- function values
- closures
- host object identity
- mutable shared JS objects
- direct access to `Request`, `Response`, or interpreter internals

This is the line that keeps the model proof-friendly.

## Capabilities

Required capabilities belong in the manifest and are enforced by the runtime.

Examples:

- `env`
- `clock`
- `random`
- `crypto`
- `stderr`
- `filesystem`
- `sqlite`
- `network`
- `policy_check`

Capability declarations do two jobs:

1. They constrain what the runtime will expose to the module.
2. They tell humans and tooling what ambient power the module consumes.

The runtime should fail closed. If a module uses an undeclared capability, that is a runtime fault and a package bug.

## State Model

The manifest should make module state explicit.

Suggested options:

- `none`: no retained state
- `instance`: state is scoped to one runtime instance
- `shared`: state is shared across instances through a declared host service

`shared` should not exist in v1 unless the runtime can prove and replay it honestly. Most useful modules can live with `none` or `instance`.

## Trace And Replay

Trace and replay are not optional add-ons. They are part of the contract.

Each export should declare a trace mode:

- `auto`: arguments and result are captured by the generic trace layer
- `manual`: the backend owns its own trace events and must satisfy the same replay contract
- `disabled`: only for setup-only calls that do not participate in request-time behavior

The default should be `auto`.

A traceable extension export must satisfy one rule:

> A traced call must replay to the same observable handler behavior when given the same request and the recorded module events.

If an extension cannot satisfy that rule, it does not belong in the proofable module surface.

## Contract Extraction

Contract extraction should stay declarative.

The manifest needs the same extraction vocabulary as built-in modules:

- `env`
- `cache_namespace`
- `sql_registration`
- `scope_name`
- `durable_key`
- `durable_step`
- `durable_signal`
- `durable_producer_key`
- `schema_compile`
- `request_schema`
- `route_pattern`
- `cookie_name`
- `cors_origin`
- `rate_limit_key`
- `service_call`

The compiler should read this metadata directly from the manifest. It should not need backend-specific code to understand what a module call contributes to the contract.

## Return Labels And Flow Proofs

Return labels are one of zigttp's best ideas. Keep them.

They let the flow checker track:

- `secret`
- `credential`
- `user_input`
- `config`
- `internal`
- `external`
- `validated`

Extension exports should declare return labels in the manifest. The flow checker should treat imported extensions and built-ins the same way once the manifest is loaded.

This matters for proofs like:

- no secret leakage
- no credential leakage
- validated input before egress
- containment of user input

## Failure Semantics

The compiler needs a small closed set of failure shapes.

Keep the current model:

- `none`
- `expected`
- `critical`
- `upstream`

The runtime backend must map its own failures into the declared shape. The manifest is not documentation here. It is part of the proof boundary.

## Laws

Every extension package must obey these laws.

### Metadata Law

Runtime behavior must match the declared manifest.

If an export claims `optional_string`, it cannot return arbitrary objects or throw undeclared shapes.

### Capability Law

A module may observe or mutate only through declared capabilities.

### Effect Law

- `none` may not observe ambient state or mutate external state.
- `read` may observe external state but may not mutate it.
- `write` may mutate external state.

### Trace Law

A traceable export must round-trip through trace and replay without changing handler-visible behavior.

### Contract Law

Only declared extraction rules and flags may affect compiler-derived sandboxing and handler contracts.

### State Law

State must follow the declared state model. Hidden shared state is a contract violation.

### Determinism Law

Any nondeterminism must enter only through declared capabilities such as `clock` or `random`.

## Registry Model

The registry should move from a compile-time array to a validated package registry.

The compiler should load manifests, validate them, and build one internal registry that covers:

- built-in modules
- packaged extensions

The rest of the compiler should query that registry uniformly.

This change also removes the need to special-case `zigttp:*` in proof passes.

## Lookup Rules

Every compiler pass should look up module metadata by:

- module specifier
- export name

Never by export name alone.

That fixes naming collisions and keeps extension modules composable.

## Backend Adapters

Each backend adapter should implement the same runtime trait:

- load package artifact
- resolve exported function
- marshal arguments from canonical values
- invoke export
- marshal result back to canonical value
- expose capability imports
- expose trace hooks
- honor state model

The compiler should never branch on backend type once the manifest is accepted.

## Migration Plan

### Phase 1

Normalize the current model.

- Stop special-casing `zigttp:*` in proof passes.
- Replace unqualified function lookup with `(specifier, export_name)` lookup.
- Make internal and external metadata schemas match exactly.

### Phase 2

Define the manifest and canonical value model.

- Add manifest parsing and validation.
- Generate SDK bindings from the manifest schema.
- Keep the Zig backend as the first implementation target.

### Phase 3

Add a second backend.

- Implement `wasm-component` or another stable foreign backend.
- Restrict v1 to proofable modules and the extension-safe type subset.

### Phase 4

Graduate ecosystem support.

- package discovery
- version compatibility checks
- lockfile or package pinning
- signed packages if needed

## What Not To Do

- Do not make the raw JS runtime ABI the public extension surface.
- Do not let extensions smuggle host power outside declared capabilities.
- Do not let backends define their own proof metadata ad hoc.
- Do not open runtime intrinsics to foreign modules before the resource model is clean.
- Do not weaken replay just to broaden the extension surface.

## First Concrete Refactor

If this design moves forward, the first implementation step should be modest:

1. Introduce a backend-neutral internal `ResolvedModuleBinding`.
2. Key all metadata lookup by `(specifier, export_name)`.
3. Remove proof-pass checks that only admit `zigttp:*`.
4. Add a manifest parser that can describe the current demo extension without changing runtime behavior.

That work pays off even before a second backend exists.

## Assumptions

- The proof system remains one of zigttp's core differentiators.
- Trace and replay stay mandatory for request-time extension calls.
- The extension ecosystem should grow by standardizing the contract, not by exposing more runtime internals.
