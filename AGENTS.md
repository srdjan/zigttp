# Repository Guidelines

## Project Structure & Module Organization
- `build.zig` defines the build graph, executables, and test steps.
- `src/` contains the runtime and server implementation (`main.zig`, `server.zig`, `zruntime.zig`).
- `zts/` is the pure-Zig JavaScript engine (parser, VM, GC, value system, JIT, modules).
- `zts/modules/` implements virtual modules (`zigttp:env`, `zigttp:crypto`, `zigttp:router`, `zigttp:auth`, `zigttp:validate`, `zigttp:cache`).
- `zts/jit/` contains the baseline JIT compiler for x86-64 and ARM64.
- `zts/parser/` contains the Pratt parser, tokenizer, IR, bytecode codegen, and scope tracking.
- `tools/` contains build-time tooling (`precompile.zig` for handler bytecode embedding).
- `examples/` holds runnable handlers and demos (`.js`, `.jsx`, `.ts`, `.tsx`).
- `scripts/` contains shell scripts for build and setup.
- `docs/` contains user-facing documentation (7 files - see Documentation section below).
- `zig-out/` and `.zig-cache/` are generated outputs; don't edit or commit them.

## Documentation

| File | Purpose |
|------|---------|
| `docs/user-guide.md` | Complete handler API reference, routing, virtual modules, CLI options |
| `docs/architecture.md` | System design, runtime model, project structure |
| `docs/jsx-guide.md` | JSX/TSX usage and server-side rendering |
| `docs/typescript.md` | Type stripping, compile-time evaluation (`comptime()`) |
| `docs/performance.md` | Benchmarks, cold starts, optimizations, deployment patterns |
| `docs/feature-detection.md` | Unsupported feature detection matrix (53 parser features, 1 stripper feature) |
| `docs/api-reference.md` | Zig embedding API, extending with native functions |

## Build, Test, and Development Commands
- `zig build` - debug build.
- `zig build -Doptimize=ReleaseFast` - optimized release build.
- `zig build -Doptimize=ReleaseFast -Dhandler=handler.jsx` - production build with embedded bytecode.
- `zig build run -- -e "function handler(r) { return Response.json({ok:true}) }"` - run with inline handler.
- `zig build run -- examples/handler.jsx -p 3000` - run a file-based handler.
- `zig build test` - all src/ and zts/ tests.
- `zig build test-zts` - JS engine tests only.
- `zig build test-zruntime` - runtime tests only (`src/zruntime.zig`).
- `zig build bench` - Zig-native benchmark suite (`src/benchmark.zig`).

## Coding Style & Naming Conventions
- Format Zig code with `zig fmt` and follow existing patterns.
- Zig identifiers: types in `UpperCamelCase`, functions and variables in `lowerCamelCase`.
- Files are short, descriptive, and lowercase (e.g., `server.zig`, `zruntime.zig`).
- Keep APIs explicit: the project uses `Result(T)`-style error handling across the engine/runtime.

## Testing Guidelines
- Tests live alongside code using Zig `test "..."` blocks (no separate test directory).
- Name tests with concise behavioral descriptions (e.g., `test "runtime init and deinit"`).
- Add tests near the feature you touched in `src/` or `zts/` and run the relevant `zig build test*` step.

## Commit & Pull Request Guidelines
- Commit history is informal; keep subjects short and descriptive (lowercase is common). Use `WIP-#:` only for intentional multi-step series.
- PRs should include: a clear summary, rationale, test commands run, and doc/example updates when behavior changes.

## Security & Configuration Notes
- Preserve path traversal checks in `src/server.zig`.
- Runtime isolation depends on `HandlerPool`/`LockFreePool`; avoid introducing shared mutable state between requests.
