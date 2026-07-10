---
name: write-test
description: "Write a test case for a handler path. Usage: /template:write-test <description>"
---
Write a test case for: {{args}}. Read the handler and adjacent tests first, derive the JSONL from compiler-proven behavior, then submit the complete test file through one `apply_edit` call.
