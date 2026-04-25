# zigttp Documentation

Use this page as the entry point for the maintained docs. Public guides cover
day-to-day use. Reference docs pin exact APIs and module behavior. Internal
notes document design contracts, governance, and roadmap work.

## Start

- [User Guide](user-guide.md) - install, run handlers, routing, JSON, tests,
  and troubleshooting.
- [TypeScript](typescript.md) - type stripping, type checking, and
  `comptime()`.
- [JSX Guide](jsx-guide.md) - JSX/TSX handlers and server-side rendering.
- [Deploy](deploy-tutorial.md) - `zigttp deploy`, sign-in, updates, and
  self-hosted control-plane expectations.

## Reference

- [Virtual Modules](virtual-modules/README.md) - canonical module index and
  per-module API pages.
- [Verification](verification.md) - compile-time handler checks and generated
  path tests.
- [Sound Mode](sound-mode.md) - type-directed truthiness, arithmetic, and
  comparison diagnostics.
- [Feature Detection](feature-detection.md) - supported and rejected language
  features.
- [Performance](performance.md) - benchmarks, cold starts, memory, and
  deployment tuning.
- [Blog: URL Shortener](blog/url-shortener-in-133-lines.md) - worked example
  with guard composition and module usage.

## Internals

- [Architecture](internals/architecture.md) - runtime, engine, request flow,
  contracts, and deploy architecture.
- [Zig Embedding API](internals/api-reference.md) - advanced server embedding
  and native function extension notes.
- [Module Capabilities](internals/capabilities.md) - built-in module
  capability governance and audit checks.
- [Control Plane Contract](internals/control-plane-contract.md) - deploy
  control-plane wire contract.
- [zigts Expert Contract](internals/zigts-expert-contract.md) - stable
  structured-tool surface for compiler-in-the-loop workflows.

## Design And Roadmap

- [Extension Model](design/extension-model.md) - third-party virtual-module
  design direction.
- [Virtual Modules Peer Package](design/virtual-modules-peer-package.md) -
  module extraction design note.
- [Virtual-Module SDK API](design/virtual-modules-sdk-api.md) - SDK inventory
  and phase-0 design.
- [Policy Wasm Capability Gating](design/zigttp_zigts_policy_wasm_spec.md) -
  policy component proposal.
- [Frontier](roadmap/frontier.md) - strategic direction.
- [Autoloop](roadmap/autoloop.md) - property-goal repair loop.
- [TUI Roadmap](roadmap/roadmap-tui.md) - pi TUI product note.
- [pi OpenAI Cassettes](roadmap/pi-cassettes.md) - provider cassette plan.

## Legacy

- [The zigttp Book](legacy/mini-book.md) is retained for older narrative
  context. Prefer the guides above for current API details.
