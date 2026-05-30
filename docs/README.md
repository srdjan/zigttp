# zigttp Documentation

Use this page as the entry point for the maintained docs. Public guides cover
day-to-day use. Reference docs pin exact APIs and module behavior. Internal
notes document architecture, governance, and audit material.

## Start

- [User Guide](user-guide.md) - install, run handlers, routing, JSON, tests,
  and troubleshooting.
- [Getting Started](getting-started.md) - zero to local deploy, covered by
  `zig build smoke-getting-started`.
- [Language Subset](language-subset.md) - supported ZigTS syntax, unsupported
  JavaScript/TypeScript features, and replacements.
- [Reading the Proof Card](proof-card.md) - the verdict, proven surface, and
  counterexamples that `check` and `dev` print on every save.
- [Proof Gate](proof-gate.md) - `zigttp proofs gate` and the GitHub Action
  that post a behavioral verdict for every changed handler on a pull request.
- [TypeScript](typescript.md) - type stripping, type checking, and
  `comptime()`.
- [JSX Guide](jsx-guide.md) - JSX/TSX handlers and server-side rendering.
- [Deploy](deploy-tutorial.md) - local `zigttp deploy`, proof review, and
  attestation.

## Reference

- [CLI Reference](cli.md) - the five-command workflow plus advanced analyzer,
  edge, proofs ledger, verify, and optional Studio commands.
- [Contracts and Auto-Sandboxing](contracts-and-sandboxing.md) -
  contract extraction, runtime sandboxing, policy override, OpenAPI
  and TypeScript SDK emit, deterministic replay, proven evolution,
  `Spec<...>`, external enrichment flags.
- [Virtual Modules](virtual-modules/README.md) - canonical module index and
  per-module API pages.
- [Verification](verification.md) - compile-time handler checks and generated
  path tests.
- [Witnesses](witnesses.md) - persisted counterexample corpus, the
  `zigttp witnesses` CLI, the studio detail endpoint, and the
  `pi_witnesses` agent tool.
- [Counterexamples](counterexamples.md) - the live HUD Counterexample
  block that fires on a regressed property: where the regression lives,
  what to do about it, and how the same payload reaches studio.
- [Sound Mode](sound-mode.md) - type-directed truthiness, arithmetic, and
  comparison diagnostics.
- [Feature Detection](feature-detection.md) - supported and rejected language
  features.
- [Restrictions to Proofs](restrictions-to-proofs.md) - every rejected
  feature mapped to the failure class it eliminates and the proof it
  unlocks. Generated from the same source as `zigts restrictions`.
- [Performance](performance.md) - benchmarks, cold starts, memory, and
  deployment tuning.
- [Limits and Failure Behavior](reliability.md) - request, memory, and stack
  limits, and how the runtime reports handler errors, faults, and exit codes.

## Internals

- [Architecture](internals/architecture.md) - runtime, engine, request flow,
  contracts, and deploy architecture.
- [Zig Embedding API](internals/api-reference.md) - advanced server embedding
  and native function extension notes.
- [Module Capabilities](internals/capabilities.md) - built-in module
  capability governance and audit checks.
- [zigts Expert Contract](internals/zigts-expert-contract.md) - stable
  structured-tool surface for compiler-in-the-loop workflows.
