---
name: handler-scaffold
description: Scaffold a minimal zigts handler with typed request parsing and error handling.
---
Create a minimal zigts handler following these conventions:
- Read nearby handlers and call `zigts_expert_modules` before choosing imports.
- Use explicit `Request` and `Response` types and add a narrow `Spec<...>` when the handler mutates state or uses effects.
- Use typed request parsing only when the route needs it; otherwise keep the scaffold small.
- All errors must be returned as structured JSON responses, never thrown.
- Import only the specific zigttp modules you need.
- Use `zigttp:decode` for request parsing and `zigttp:validate` for schema validation. Check every `Result.ok` before reading `.value`.
- Use `Response.json()` for all JSON responses, `Response.text()` for plain text.
- No classes, no async/await, no try/catch. Use `match` for error branching.
- Run `zigts_expert_verify_paths` before editing when modifying an existing file, and let the compiler veto verify the complete scaffold before apply.

Start with the minimal passing scaffold and add complexity only as needed.
