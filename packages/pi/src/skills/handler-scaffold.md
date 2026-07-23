---
name: handler-scaffold
description: Scaffold a minimal zts handler with typed request parsing and error handling.
---
Create a minimal zts handler following these conventions:
- Read nearby handlers and call `zts_expert_modules` before choosing imports.
- Use explicit `Request` and `Response` types and add a narrow `Spec<...>` when the handler mutates state or uses effects.
- Use typed request parsing only when the route needs it; otherwise keep the scaffold small.
- All errors must be returned as structured JSON responses, never thrown.
- Import only the specific zttp modules you need.
- Use `zttp:decode` for request parsing and `zttp:validate` for schema validation. Check every `Result.ok` before reading `.value`.
- Use `Response.json()` for all JSON responses, `Response.text()` for plain text.
- No classes, no async/await, no try/catch. Use `match` for error branching.
- Run `zts_expert_verify_paths` before editing when modifying an existing file, and let the compiler veto verify the complete scaffold before apply.

Start with the minimal passing scaffold and add complexity only as needed.
