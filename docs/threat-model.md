# Threat Model

Threats specific to the rule review system and Claude Code integration. For runtime isolation and handler sandboxing, see [verification.md](verification.md) and the sandboxing section of [mini-book.md](mini-book.md).

## Threats

| Threat | Vector | Mitigation | Residual Risk |
|--------|--------|------------|---------------|
| Dev modifies hook scripts | Attacker with repo write access changes `.claude/hooks/*.sh` or `.claude/settings.json` to suppress violation reports or exfiltrate data | Three hooks installed by `zigts init`: `pre-edit-zts.sh` (PreToolUse, advisory), `post-edit-zts.sh` (PostToolUse, advisory), `session-start.sh` (SessionStart, env export). Canonical sources live in `packages/tools/src/hooks/` and are embedded in the binary. `permissions.deny` prevents Claude from editing hooks or rule_registry.zig. Code review catches settings.json changes. `zigts init --force` restores canonical hooks from the binary. | If merged uncaught, hook could suppress violation reports silently. Deny rules only apply to Claude's built-in file tools, not to Bash subprocesses. |
| `CLAUDE_ENV_FILE` injection | Attacker sets the `CLAUDE_ENV_FILE` env var to point at a malicious file that injects env vars into the Claude Code session | zigts reads only from explicit `--stdin-json` pipe or file arguments; no env file reading in the policy system; the rule engine has no env-file feature | Requires process-level env manipulation, outside zigts scope |
| Policy hash tampering | Attacker updates `policy-hash.txt` to match silently modified rule definitions | CI asserts hash on every release build; PR review catches `policy-hash.txt` changes alongside rule changes; hash is derived from all rule metadata | Coordinated multi-file change could bypass if reviewer misses both files |
| Substring heuristic bypass | Crafted stub JSON that contains `"ok":false` as a substring within a value field, not as a structural field | Annotated as v0.1 placeholder in property_diagnostics.zig; structured comparison planned for v0.2 | False positive/negative in counterexample matching until v0.2 |
| Temp file races | Concurrent edit-simulate invocations write to /tmp with predictable names, enabling symlink attacks | Temp file names include a stack-address-derived suffix for uniqueness; files are deleted immediately after use; analysis runs as the user's own process | Low risk on single-user dev machines; not designed for multi-tenant servers |
