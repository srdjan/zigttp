# Release Checklist

Reusable checklist for cutting a zigttp release. Fill in the version at the
top and work through each section in order.

## Release metadata

- Version: ______ (from build.zig.zon line 3)
- Previous tag: ______ (`git describe --tags --abbrev=0`)
- Zig toolchain: ______ (from build.zig.zon `minimum_zig_version`)

## 1. Pre-release validation

Run every gate on macOS (native) before tagging. All must pass.

- [ ] `zig build test` -- all unit tests (src/ + zigts/ + tools/)
- [ ] `zig build test-zigts` -- engine tests only
- [ ] `zig build test-zruntime` -- runtime tests only
- [ ] `bash scripts/test-examples.sh` -- 14 example handler suites (exit 0)
- [ ] `zig build -Doptimize=ReleaseFast` -- release binary compiles cleanly

## 2. Cross-compile verification

SQLite3 is vendored as a static amalgamation (`deps/sqlite/`), enabling cross-compilation.

- [ ] `zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux-gnu` -- Linux x86_64
- [ ] `zig build -Doptimize=ReleaseFast -Dtarget=aarch64-linux-gnu` -- Linux ARM64
- [ ] Verify binary sizes are reasonable (`ls -lh zig-out/bin/`)

## 3. Release build

- [ ] `zig build release` -- produces zig-out/bin/zigttp and zig-out/bin/zigts
- [ ] Smoke-test the server binary:
  - `./zig-out/bin/zigttp -e "function handler(r) { return Response.json({ok:true}) }" &`
  - `curl http://localhost:3000` returns `{"ok":true}`
  - Kill the background process
- [ ] Smoke-test the compiler CLI:
  - `./zig-out/bin/zigts check examples/handler/handler.ts`

## 4. Known issues

Review open Critical/High items in TODO.md before each release. These ship
as acknowledged known issues unless the team decides to block on them.

- [x] Closure data freed as BytecodeFunctionData (TODO.md Critical) - fixed: destroyFull closure discriminator
- [x] Chunked transfer encoding not handled (TODO.md Critical) - fixed: reject with 501
- [x] Bytecode cache is trusted input (TODO.md Critical) - fixed: validateBytecode after deserialization
- [x] Static file path traversal via symlinks (TODO.md Critical) - fixed: check-before-open + follow_symlinks=false
- [x] HandlerPool test flakes under build runner (TODO.md Medium) - fixed: root cause was closure destroyFull bug

If any were fixed since the last release, check the box and note the commit.

## 5. Version and tag

- [ ] Confirm `build.zig.zon` `.version` matches the intended release
- [ ] `git tag -a vX.Y.Z -m "zigttp vX.Y.Z"`
- [ ] `git push origin vX.Y.Z`

## 6. Post-release verification

- [ ] `git checkout vX.Y.Z && zig build test` -- clean build from tag
- [ ] `zig build -Doptimize=ReleaseFast` -- release binary from tag
- [ ] Smoke-test server from tagged build
- [ ] Update any external docs or site that reference the version number

## 7. GitHub Release (automated)

Pushing a tag triggers `.github/workflows/release.yml`, which runs tests on macOS and
Linux, cross-compiles release binaries for all four targets, and creates a GitHub Release
with tarballs and SHA-256 checksums. Verify the workflow completes and the release page
has all expected assets.

## Notes

- CI runs on tag push via `.github/workflows/release.yml`.
- macOS and Linux are the supported platforms. No Windows target.
- The `release` build step produces both `zigttp` (server) and `zigts` (compiler/analyzer CLI).
- Cross-compilation uses Zig's built-in `-Dtarget` flag. No Docker or remote build needed.
- SQLite3 is vendored as a static amalgamation in `deps/sqlite/`.
