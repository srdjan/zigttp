# Contributing

Thanks for your interest in zigttp. This document covers the practical bits: how to build, how to test, and the conventions a PR needs to respect before merge.

For agent-facing conventions (Claude Code, hooks, skills) see [AGENTS.md](AGENTS.md) and [CLAUDE.md](CLAUDE.md).

## Toolchain

Validated on Zig `0.16.0-dev.3073+28ae5d415`. Newer nightlies are best-effort until revalidated. Use the exact version in `build.zig.zon`'s `minimum_zig_version` if you hit confusing build errors.

## Build

```bash
zig build                              # debug
zig build -Doptimize=ReleaseFast       # release
zig build -Dhandler=handler.ts         # precompile a handler
```

## Test

```bash
zig build test                         # all unit + integration suites
zig build test-zigts                   # engine only
zig build test-zruntime                # runtime only
bash scripts/test-examples.sh          # end-to-end example handlers
zig build bench                        # Zig-native microbenchmarks
```

Run the relevant `test*` step before opening a PR. If you touched the compile-time checkers or the rule registry, also run:

```bash
zig build release
./zig-out/bin/zigts describe-rule --hash   # must match policy-hash.txt
```

Larger benchmarks live in the sibling repo `../zigttp-bench`; do not add benchmark scripts here.

## Adding a virtual module

Virtual modules live in `packages/zigts/src/modules/`. Each module must:

1. Declare a `ModuleBinding` in `module_binding.zig` with explicit `required_capabilities` (clock, crypto, random, stderr, sqlite, filesystem, network, env, runtime_callback, policy_check).
2. Enter and leave the active-module context via the shared helpers in `module_binding.zig`; the `test-capability-audit` build step enforces this.
3. Annotate each exported function with its effect class (read / write / none) so contract extraction can derive handler properties.
4. Ship fixtures under `tests/validate/` or an example under `examples/` covering both success and failure paths.

## Adding a compile-time rule

Rules live in the checker cluster (`type_checker.zig`, `flow_checker.zig`, `fault_coverage.zig`, `bool_checker.zig`, `handler_contract.zig`) and are surfaced through `zigts describe-rule`. When you add or remove a rule:

1. Update the corresponding checker and regenerate the rule registry if needed.
2. Run `./zig-out/bin/zigts describe-rule --hash > policy-hash.txt` and commit the new hash.
3. Add a test case under `tests/verify/` that exercises the diagnostic end-to-end.

## Code style

- `zig fmt` before every commit. CI does not auto-format.
- Types `UpperCamelCase`, functions/variables `lowerCamelCase`, files lowercase (`server.zig`, `zruntime.zig`).
- Prefer `Result(T)` over exceptions for expected failures across the engine/runtime.
- `errdefer` every allocation. `orelse` over `?` unwrap on hot paths.
- No `catch unreachable` on request paths. If the invariant is real, return a typed error and handle it.
- Do not introduce shared mutable state between pool workers; see `HandlerPool` / `LockFreePool`.

## Commits and pull requests

- Keep commit subjects short, lowercase, and descriptive (`feat(deploy): ...`, `fix(parser): ...`). The repo already uses Conventional Commits loosely.
- Each PR should have: a one-line summary, rationale, the `zig build test*` commands you ran, and doc or example updates if behavior changed.
- Update [CHANGELOG.md](CHANGELOG.md) under the `[Unreleased]` section for any user-visible change. Internal refactors can be omitted.
- Do not commit generated output (`zig-out/`, `.zig-cache/`).
- Do not commit secrets, credentials, or anything under `~/.zigttp/`.

## Reporting bugs

For non-security bugs, open a GitHub issue with a minimal reproducer (handler source, CLI flags, the actual vs. expected output). For security reports, follow [SECURITY.md](SECURITY.md).
