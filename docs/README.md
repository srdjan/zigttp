# Documentation

These docs describe the current codebase. Release history lives in
`CHANGELOG.md`; plans and release snapshots stay out of the maintained docs
path.

## Start Here

- [User Guide](user-guide.md) - the single user guide for install, first
  project, handler API, routing, JSON, JS/TS/TSX, virtual modules, tests,
  deploy, proof receipts, and troubleshooting.
- [CLI Reference](cli.md) - core commands, advanced analyzer commands,
  proof-ledger commands, expert mode, and machine-readable output.
- [Virtual Modules](virtual-modules/README.md) - complete current `zigttp:*`
  module list, exports, capabilities, effects, and runtime requirements.
- [Roadmap](roadmap.md) - one forward-looking document for supported platforms,
  current limitations, and planned work.

## Reference

- [Contracts and Auto-Sandboxing](contracts-and-sandboxing.md) - handler
  contracts, least-privilege runtime policy, OpenAPI/SDK emit, replay,
  upgrade checks, and `Spec<...>`.
- [Verification](verification.md) - compile-time handler checks.
- [TypeScript](typescript.md) - type stripping, type checking, TSX, and
  `comptime()`.
- [TypeScript Patterns](typescript-patterns.md) - the "TypeScript Tips Everyone
  Should Know" canon mapped onto the zigts subset.
- [Feature Detection](feature-detection.md) - allowed and rejected language
  features.
- [Restrictions to Proofs](restrictions-to-proofs.md) - why each language cut
  exists.
- [Sound Mode](sound-mode.md) - type-directed truthiness, arithmetic, and
  comparison diagnostics.
- [Canonical Profile](canonical-profile.md) - canonical ZigTS rules and
  `zigttp normalize`.
- [Proof Card](proof-card.md), [Proof Gate](proof-gate.md),
  [Counterexamples](counterexamples.md), and [Witnesses](witnesses.md) -
  proof UI, pull-request gates, failing inputs, and persisted witness corpus.
- [Edge Runtime](edge.md) - optional in-process multi-handler router.
- [Performance](performance.md) - current benchmark claims and tuning notes.
- [Reliability](reliability.md) - limits, failure behavior, and exit codes.
- [Threat Model](threat-model.md) - current trust boundaries and non-goals.

## Internals

- [Architecture](internals/architecture.md) - runtime, engine, request flow,
  contracts, and deploy architecture.
- [Zig Embedding API](internals/api-reference.md) - advanced Zig embedding and
  native function extension notes.
- [Module Capabilities](internals/capabilities.md) - built-in module
  capability governance.
- [zigts Expert Contract](internals/zigts-expert-contract.md) - stable
  structured-tool output used by compiler-in-the-loop workflows.
