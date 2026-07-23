# zttp unreleased

Curated release notes. Paste the sections below above the auto-generated
commit changelog when editing the next GitHub Release. Mirrors `CHANGELOG.md`
under `[Unreleased]`.

The previous cycle's notes shipped in 0.17.0; this file is reset for the next
cycle. Add curated entries here as `[Unreleased]` in `CHANGELOG.md` grows.

## Highlights

## Changed

## Added

## Breaking changes

- **Project rename:** current builds use the `zttp`, `zttp-runtime`, and `zts`
  executables; `zttp:*` virtual modules; `ZTTP_*` environment and installer
  variables; the `Zttp-Attest` header; the `~/.zttp` install directory; and
  `zttp-...` release archives. Releases through v0.18.0 retain the historical
  `zigttp`, `zigttp-runtime`, `zigts`, `zigttp:*`, `ZIGTTP_*`,
  `Zigttp-Attest`, `~/.zigttp`, and `zigttp-...` names. The current installer
  recognizes those archives and exposes their executables under the current
  command names; their runtime interfaces and reported identities remain
  historical.
