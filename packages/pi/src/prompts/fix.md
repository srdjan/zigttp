---
name: fix
description: "Fix a specific violation or error. Usage: /template:fix <error>"
---
Fix the following issue in the handler: {{args}}. Read the file, run `zts_expert_verify_paths`, use `pi_repair_plan` plus `pi_goal_candidate` or `pi_apply_repair_plan` for supported repairs, then apply the smallest verified edit. Explain the root cause only after the compiler accepts the fix.
