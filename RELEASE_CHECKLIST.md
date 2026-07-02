# Release Checklist

Reusable checklist for cutting a zigttp release. Fill in the metadata, run the
gates, and keep release notes user-facing.

## Metadata

- Version: ______ (from `build.zig.zon`)
- Previous tag: ______ (`git describe --tags --abbrev=0`)
- Zig toolchain: ______ (from `build.zig.zon` `minimum_zig_version`)

## Validation

- [ ] `zig fmt --check build.zig packages/`
- [ ] `zig build test`
- [ ] `zig build test-zruntime`
- [ ] `zig build test-docs-drift test-doc-links` (docs registry and relative links)
- [ ] `zig build smoke-v1`
- [ ] `zig build test-panic-isolation`
- [ ] `zig build smoke-getting-started` (macOS beta gate)
- [ ] `zig build smoke-demo` (macOS beta gate)
- [ ] `zig build smoke-studio` (macOS beta gate; builds `-Dstudio`)
- [ ] `bash scripts/test-examples.sh`
- [ ] `bash scripts/test-install-archive-safety.sh`
- [ ] `zig build -Doptimize=ReleaseFast`
- [ ] `bash scripts/check-semantics-spec.sh`
- [ ] `zig build bench-check` (advisory; if a single benchmark misses once, rerun immediately and block only if it fails twice)
- [ ] `./zig-out/bin/zigttp doctor --release --json`

## Cross-Compile

SQLite is vendored as a static amalgamation, so release builds should
cross-compile without Docker.

- [ ] `zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux-gnu -Dstrip`
- [ ] `zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux-gnu -Dstrip`
- [ ] Check release binary sizes with `ls -lh zig-out/bin/`. The release workflow
      builds with `-Dstrip`; stripped `zigttp` is roughly 8-9 MB (vs ~50 MB
      unstripped). A debug-sized artifact in the release means `-Dstrip` was dropped.

## Documentation

- [ ] `README.md` points to `docs/README.md`.
- [ ] `docs/user-guide.md` is the only user guide.
- [ ] `docs/roadmap.md` is the only roadmap.
- [ ] `docs/virtual-modules/README.md` matches the built-in module registry.
- [ ] `docs/performance.md` contains the current public benchmark claims.
- [ ] Release notes link to `docs/user-guide.md`, `docs/cli.md`, and `examples/README.md`.
- [ ] No maintained docs point to release snapshots or stale transition notes.

## Tag And Publish

- [ ] Confirm `build.zig.zon` `.version` matches the intended release.
- [ ] Confirm `packages/zigts/src/root.zig` `version.string` matches the intended release.
- [ ] Promote `CHANGELOG.md` `[Unreleased]` to `[X.Y.Z] - <date>`, open a fresh `[Unreleased]` section, and update the bottom compare-link anchors (`[Unreleased]` base + a new `[X.Y.Z]` link).
- [ ] Draft release notes from `CHANGELOG.md` and `.github/RELEASE_NOTES_TEMPLATE.md`.
- [ ] `git tag -a vX.Y.Z -m "zigttp vX.Y.Z"`
- [ ] `git push origin vX.Y.Z`

Pushing a tag triggers `.github/workflows/release.yml`, which runs tests,
cross-compiles release binaries, and creates the GitHub Release with tarballs
and SHA-256 checksums. Verify the workflow and release assets before announcing.
