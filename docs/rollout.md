# Rollout Plan

Rule review system and Claude Code integration, phased across minor versions.

## Milestones

| Version | Deliverables |
|---------|-------------|
| v0.14.1 | `RuleError` error set, `RuleContext` struct, `rule_registry.zig` (comptime-derived from checker enums), `introduced_by_patch` field on `PolicyViolation` and `PropertyViolation`, `ArrayListUnmanaged` migration in handler_policy.zig |
| v0.14.2 | `edit-simulate` subcommand with `--stdin-json`, `describe-rule` with `--json` and `--hash`, `search` subcommand |
| v0.14.3 | `review-patch` with `--diff-only` and `ReviewFile` type, Claude Code PostToolUse hook in settings.json, `python3`/`python` deny list |
| v0.15.0 | CI policy hash assertion in release workflow, threat model doc, `build.zig` included in release tarball, rollout doc |

## Deliverable Details

**build.zig in release tarball**: Added to the Package tarball step in `.github/workflows/release.yml`. Users who build from source need this file; binary users get only `zigttp` and `zigts` executables.

**policy-hash.txt**: Committed to the repo root. Regenerated with `./zig-out/bin/zigts describe-rule --hash > policy-hash.txt` whenever rules are added, removed, or their descriptions change.
