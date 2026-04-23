---
name: handler-scaffold
description: Scaffold a minimal zigts handler with typed request parsing and error handling.
---
Create a minimal zigts handler following these conventions:
- Use typed destructuring for request params (query, body, path).
- All errors must be returned as structured JSON responses, never thrown.
- Import only the specific zigttp modules you need.
- Use `zigttp:decode` for request parsing and `zigttp:validate` for schema validation.
- Use `Response.json()` for all JSON responses, `Response.text()` for plain text.
- No classes, no async/await, no try/catch. Use `match` for error branching.
Start with the minimal passing scaffold and add complexity only as needed.
