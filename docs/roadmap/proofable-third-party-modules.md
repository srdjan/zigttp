# Proofable Third-Party Virtual Modules

Deferred roadmap item. The next release is focused on UX; this proposal stays
as the follow-on extension direction once authoring and proof inspection feel
coherent.

## Summary

Third-party virtual modules should participate in the same proof pipeline as
built-ins. A module package ships a `zigttp-module.json` manifest that declares
its exports, effects, capabilities, return kinds, traceability, contract
extraction rules, failure severity, and state model. The compiler reasons over
that manifest; the runtime enforces that the implementation matches it.

The core principle: the proof metadata is the extension contract. The execution
backend is an implementation detail.

## V1 Shape

- Support `zigttp-ext:*` imports only when a validated manifest is registered.
- Keep `zigts verify-module-manifest <manifest.json> --json` as the standalone
  validation entrypoint.
- Add manifest registration to check/build flows, for example
  `zigts check handler.ts --module-manifest path/to/zigttp-module.json` and a
  build option for one or more manifests.
- Load built-ins and registered manifests into one module catalog keyed by
  `(specifier, export_name)`.
- Use manifest metadata for type checking, flow checking, path generation,
  contract extraction, sound-mode import typing, trace/replay, and capability
  policy derivation.
- Treat `native-zig` as the first executable backend through `zigttp-sdk`;
  reserve `wasm-component` in the schema without executing it in v1.

## Expert UX

- Add an extension status command that lists registered manifests, exports,
  effects, capabilities, and validation errors.
- Teach `zigts expert` to consult the live extension catalog before suggesting
  imports from `zigttp-ext:*`.
- Add a scaffold path for a minimal native-Zig extension package: manifest,
  `src/root.zig`, build file, and a verified handler example.

## Test Plan

- Manifest parser tests for valid demo metadata, duplicate exports, invalid
  capabilities, invalid effects, and invalid return kinds.
- Integration test for a handler importing `zigttp-ext:math` with and without a
  registered manifest.
- Contract JSON round-trip test for extension manifest hash/specifier/export
  facts.
- Expert catalog test that verifies registered extension modules appear in
  module discovery.

## Assumptions

- No package registry in v1.
- No WASM execution in v1.
- Existing `zigttp:*` module behavior remains unchanged.
- Manifests fail closed: an unregistered `zigttp-ext:*` import is an error.
