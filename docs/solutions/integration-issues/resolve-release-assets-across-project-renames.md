---
title: Resolve Release Assets Across Project Renames
date: 2026-07-23
category: integration-issues
module: Shell installer and GitHub release asset resolution
problem_type: integration_issue
component: tooling
symptoms:
  - The default installer selected v0.18.0 and requested zttp-named archives that the release did not publish, causing an HTTP 404.
  - Pinning ZTTP_VERSION=v0.18.0 failed through the same missing-asset path.
  - Historical release documentation named identifiers that those releases never shipped.
root_cause: logic_error
resolution_type: code_fix
severity: high
related_components:
  - documentation
  - testing_framework
  - development_workflow
tags:
  - installer
  - github-releases
  - release-assets
  - project-rename
  - backward-compatibility
  - checksums
  - shell
---

# Resolve Release Assets Across Project Renames

## Problem

The default installer selected release v0.18.0 but assumed every selected tag
used the current `zttp-<version>-<os>-<arch>.tar.gz` asset convention. Releases
through v0.18.0 use the historical `zigttp` archive and executable names
instead, as the current unreleased note records (`docs/releases/unreleased.md:18`).
The resulting download URL did not exist, so an end user running `./install.sh`
received HTTP 404 before checksum or archive validation could begin.

A repository-wide rename had also changed identifiers inside the already
published v0.18.0 changelog entry. Historical release records must describe
what that release shipped, including its `Zigttp-Attest` header
(`CHANGELOG.md:20`).

## Symptoms

The end-user path failed immediately after version resolution:

```text
Fetching latest stable version...
Installing zttp v0.18.0 (macos/aarch64) ...
curl: (56) The requested URL returned error: 404
```

The installer never reached its integrity and archive safety checks because
those run only after both selected assets download (`install.sh:47`). Historical
binaries also report `zigttp 0.18.0` and `zigts 0.18.0`, so checks for only
current prefixes reject an otherwise valid installation. The installed-version
check accepts both prefix families (`install.sh:482`).

## What Didn't Work

Constructing the asset name solely from the selected tag, platform, and current
project name is not a reliable release contract. A tag can remain valid while
naming conventions change later. Falling back only after a 404 is also weak
because it treats network and authorization failures like compatibility signals
and can select an archive without first proving that its exact checksum
companion exists.

A prior repository-wide mechanical rename affected both the installer and
archived release prose (session history). A historical release record describes
what that release shipped, not the repository's current vocabulary. The rename
belongs in current release documentation, where the old and new interfaces can
be stated together (`docs/releases/unreleased.md:18`).

## Solution

The installer caches the GitHub release response while resolving beta, latest,
or stable channels (`install.sh:120`). A pinned version fetches its tag metadata
only when asset resolution needs it (`install.sh:187`).

Asset selection tests exact metadata names and requires a complete archive and
checksum pair. It prefers current naming and falls back to historical naming:

```sh
if release_has_asset "$RELEASE_METADATA" "${current_root}.tar.gz" &&
    release_has_asset "$RELEASE_METADATA" "${current_root}.tar.gz.sha256"; then
    PAYLOAD_ROOT="$current_root"
    ZTTP_SOURCE="zttp"
    ZTS_SOURCE="zts"
elif release_has_asset "$RELEASE_METADATA" "${legacy_root}.tar.gz" &&
    release_has_asset "$RELEASE_METADATA" "${legacy_root}.tar.gz.sha256"; then
    PAYLOAD_ROOT="$legacy_root"
    ZTTP_SOURCE="zigttp"
    ZTS_SOURCE="zigts"
fi
```

The exact asset-name parser and exhaustive current-or-historical selection are
in `install.sh:178`. If neither complete pair exists, installation fails with
the expected alternatives instead of guessing (`install.sh:207`).

The selected payload source names are staged into the current destination
commands `zttp`, `zts`, and `zttp-runtime` (`install.sh:64`). This keeps the
current installation surface stable without rewriting the historical binary
payload or its reported identity. Existing installations are recognized under
either current or historical version prefixes (`install.sh:482`).

The regression test creates a checksum-protected fake v0.18.0 archive with
`zigttp`, `zigts`, and `zigttp-runtime` payloads
(`scripts/test-install-archive-safety.sh:65`). It drives `main` through metadata
lookup, download, checksum validation, extraction, and transactional
installation (`scripts/test-install-archive-safety.sh:104`). Assertions verify
the current destination paths, historical version outputs, absence of old
destination names, and a second run that exits as already installed without
network access (`scripts/test-install-archive-safety.sh:140`).

The historical changelog keeps the identifiers shipped by released versions.
The rename boundary is documented under the current unreleased release notes
(`docs/releases/unreleased.md:18`).

## Why This Works

Release metadata is the source of truth for the assets currently attached to
the selected release.
Exact-name matching avoids substring collisions, while requiring the
corresponding `.sha256` asset preserves the installer's integrity boundary
(`install.sh:178`). The chosen payload root is also passed to archive path
validation, so compatibility does not weaken extraction safety
(`install.sh:50`, `install.sh:609`).

Separating source binary names from destination command names makes the
compatibility rule explicit and local. The archive remains historically
accurate, but users receive the current command paths (`install.sh:71`).
Accepting both version-output families prevents repeated downloads of valid
historical binaries while still requiring the selected version
(`install.sh:482`).

The real default install succeeded against GitHub v0.18.0 on macOS/aarch64, and
the regression test covers the same release shape without depending on mutable
network state.

## Prevention

- Resolve release artifacts from the selected release's metadata.
- Require the archive and its exact checksum companion before selecting either.
- Publish a new naming convention before installer defaults adopt it.
- Keep released changelog sections immutable and document identifier cutovers
  under Unreleased or the release that introduces them.
- Test historical archive layouts through the same installer entry point users
  run, including already-installed detection.

Run the focused compatibility checks:

```sh
sh -n install.sh scripts/test-install-archive-safety.sh
shellcheck install.sh scripts/test-install-archive-safety.sh
bash scripts/test-install-archive-safety.sh
zig build test-docs-drift test-doc-links
git diff --check
```

Exercise the actual end-user installation path in a disposable directory:

```sh
ZTTP_INSTALL_DIR="$(mktemp -d)/install" ./install.sh
```

Run the full repository gate before release:

```sh
bash scripts/verify.sh
```

## Related Issues

- [v0.18.0 release assets](https://github.com/srdjan/zigttp/releases/tag/v0.18.0)
- [Self-extract runtime policy attestation binding](../security-issues/self-extract-runtime-policy-attestation-binding.md)
