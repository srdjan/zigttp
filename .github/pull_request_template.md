<!-- One-line summary of the change. -->

## Summary

## Rationale

<!-- Why this change is needed. Link any related issue. -->

## Tests run

<!-- The `zig build test*` commands you ran locally, and their result. -->

- [ ] `zig build test`
- [ ] `zig build test-zigts` / `test-zruntime` (if engine or runtime touched)
- [ ] `bash scripts/test-examples.sh` (if handler-facing behavior changed)

## Checklist

- [ ] `zig fmt` run on all changed Zig files.
- [ ] `CHANGELOG.md` updated under `[Unreleased]` for any user-visible change.
- [ ] Docs or examples updated if behavior changed.
- [ ] If compile-time rules changed: `policy-hash.txt` regenerated and committed.
- [ ] No secrets, credentials, or generated output committed.
