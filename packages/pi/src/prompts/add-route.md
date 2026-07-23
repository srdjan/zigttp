---
name: add-route
description: "Add a new route to the handler. Usage: /template:add-route <METHOD> <path>"
---
Add a {{1}} {{2}} route to the current handler. Use the compiler-native Route Forge path first: read the target file, run `zts_expert_verify_paths`, call `pi_forge_route`, then submit its returned `proposed_content` as one `apply_edit` call. If Route Forge reports a blocker, make the smallest manual edit and let the compiler veto verify it.
