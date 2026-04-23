---
name: fix-violations
description: Fix all compiler violations in the current file by iterating until clean.
---
Work through every compiler violation in the current handler file systematically:
1. Run `/check` to get the full violation list.
2. Fix the highest-priority violations first (ZTS001-ZTS099 before ZTS100+).
3. After each edit, run `/verify <file>` to confirm the fix landed.
4. Continue until `/check` reports zero violations.
Do not introduce new violations while fixing existing ones. Use `--diff-only` mode.
