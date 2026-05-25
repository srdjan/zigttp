# CLI Tools Extraction (deferred to v0.2.0)

## Why this is deferred

The original v0.1.0 plan called for extracting dev-only proof tooling from `packages/runtime/` into a sibling `packages/cli-tools/` package, on the assumption that the four `proof_*` files were standalone:

- `proof_card_tui.zig` (1,565 LOC)
- `proof_quest.zig` (414 LOC)
- `proof_ledger.zig` (461 LOC)
- `proof_audit_ring.zig` (174 LOC)

A direct read of the imports surfaced two facts that rule the extraction out of v0.1.0 scope:

1. **`proof_audit_ring.zig` is not dev-only.** `server.zig` pushes route-blocked and cache-hit events into it on every request (`packages/runtime/src/server.zig:625, 639, 1978, 1997`). It belongs in the runtime package and stays there.

2. **The other three files couple into `deploy/` and `cli_shared`.**
   - `proof_card_tui.zig` imports `deploy/review.zig`.
   - `proof_ledger.zig` imports `deploy/review.zig`, `deploy/state.zig`, `deploy/json_util.zig`.
   - `proof_quest.zig` imports `zigts_cli` and `cli_shared.zig`.

   Moving just the three files would either drag the entire 22-file / ~8.5 KLoC `deploy/` subtree with them or require a non-trivial dependency-injection refactor of the proof modules.

The `deploy/` subtree is itself a candidate for extraction (see also `docs/roadmap/deploy-extraction.md` once written), and these proof modules should ride that same change.

## The seam

The proof modules consume two distinct surfaces from `deploy/`:

- **JSON I/O helpers** — `deploy/json_util.zig` is a generic JSON utility module; trivially extractable.
- **Deploy review domain types** — `deploy/review.zig` and `deploy/state.zig` carry data structures the proof viewers render. The right cut is probably to push those types into a small, neutral `proof_types.zig` (or `attest_types.zig`) module that both `deploy/` and `proof_*` modules import, leaving `deploy/` itself as the producer.

`cli_shared.zig` should split: the parts used by `proof_quest.zig` (argument parsing, exit codes) belong with cli-tools; the parts used by runtime CLI startup (watch-set management, size parsing) stay with runtime.

## v0.2.0 sequence

1. Define `packages/runtime/src/proof_types.zig` (or `packages/zigts/src/attest_types.zig`, depending on where the types live most naturally) and migrate the structs the proof modules need.
2. Refactor `proof_card_tui.zig`, `proof_ledger.zig`, `proof_quest.zig` to import from the neutral types module instead of `deploy/`.
3. Split `cli_shared.zig` into `cli_args.zig` (extracted) and `cli_runtime.zig` (kept).
4. Create `packages/cli-tools/` with its own `build.zig` / `build.zig.zon`, move the three files plus the extracted shared module.
5. Update `dev_cli.zig` imports; verify `zigttp` binary still links the proof viewers and `zigttp-runtime` still does not.
6. Move the rest of `deploy/` to `packages/cli-tools/deploy/` (or its own package) once `--cloud` deploy is ready to ship.

## What's in v0.1.0 instead

- `proof_audit_ring.zig` stays in runtime and is documented in the threat model.
- The three dev-only proof modules continue to live in `packages/runtime/src/` and are reachable only through `dev_cli.zig`. The build graph already excludes them from the `zigttp-runtime` binary — verified by:

  ```bash
  nm zig-out/bin/zigttp-runtime | grep -E "proof_card_tui|proof_quest|proof_ledger"
  # expected: empty
  ```

  So the "do dev tools land in the server binary?" question is already answered "no" at link time. The motivation for the file move is purely organizational; the user-visible cost (binary size, attack surface) is zero today.
