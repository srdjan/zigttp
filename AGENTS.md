# Repository Guidelines

## Project Structure & Module Organization
- `build.zig` defines the build graph, executables, and test steps.
- `src/` contains the runtime and server implementation (`main.zig`, `server.zig`, `zruntime.zig`).
- `zts/` is the pure-Zig JavaScript engine (parser, VM, GC, value system, etc.).
- `examples/` holds runnable handlers and demos (`.js`, `.jsx`).
- `docs/` contains user-facing documentation.
- `zig-out/` and `.zig-cache/` are generated outputs; don’t edit or commit them.

## Build, Test, and Development Commands
- `zig build` — debug build.
- `zig build -Doptimize=ReleaseFast` — optimized release build.
- `zig build run -- -e "function handler(r) { return Response.json({ok:true}) }"` — run with inline handler.
- `zig build run -- examples/handler.jsx -p 3000` — run a file-based handler.
- `zig build test` — unit tests for the main runtime.
- `zig build test-zts` — unit tests for the JS engine.
- `zig build test-zruntime` — unit tests for `src/zruntime.zig`.
- `./setup.sh` — convenience script that builds a release binary.

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
