# Release Checklist

Reusable checklist for cutting a zigttp release. Fill in the metadata, run the
gates, and keep release notes user-facing.

## Metadata

- Version: ______ (from `build.zig.zon`)
- Previous tag: ______ (`git describe --tags --abbrev=0`)
- Zig toolchain: ______ (from `build.zig.zon` `minimum_zig_version`)

## Validation

- [ ] `zig build test`
- [ ] `zig build test-zigts`
- [ ] `zig build test-zruntime`
- [ ] `zig build test-docs-drift test-doc-links` (docs registry and relative links)
- [ ] `zig build smoke-v1`
- [ ] `zig build smoke-getting-started` (macOS beta gate)
- [ ] `zig build smoke-demo` (macOS beta gate)
- [ ] `zig build smoke-studio` (macOS beta gate; builds `-Dstudio`)
- [ ] `bash scripts/test-examples.sh`
- [ ] `zig build -Doptimize=ReleaseFast`
- [ ] `./zig-out/bin/zigttp doctor --release --json`

## Cross-Compile

SQLite is vendored as a static amalgamation, so release builds should
cross-compile without Docker.

- [ ] `zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux-gnu`
- [ ] `zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux-gnu`
- [ ] Check release binary sizes with `ls -lh zig-out/bin/`.

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
- [ ] Draft release notes from `CHANGELOG.md` and `.github/RELEASE_NOTES_TEMPLATE.md`.
- [ ] `git tag -a vX.Y.Z -m "zigttp vX.Y.Z"`
- [ ] `git push origin vX.Y.Z`

Pushing a tag triggers `.github/workflows/release.yml`, which runs tests,
cross-compiles release binaries, and creates the GitHub Release with tarballs
and SHA-256 checksums. Verify the workflow and release assets before announcing.
