# Security Policy

## Supported Versions

Only the latest tagged release and `main` receive security fixes. Release candidates (`-rcN`) are supported until the corresponding final release ships.

## Reporting a Vulnerability

Report suspected vulnerabilities privately via GitHub Security Advisories:

https://github.com/srdjan/zigttp/security/advisories/new

Please do not open public issues for security reports. Expect an initial acknowledgment within a few business days. After triage you will receive a remediation plan or a rationale for closing the report.

Include, where possible:

- Affected version (tag, commit, or binary hash)
- Minimal reproducer (handler source, CLI flags, request)
- Observed vs. expected behavior
- Impact assessment (panic, isolation bypass, data exposure, etc.)

## Scope

In scope: the runtime (`packages/runtime/`), the zigts engine and virtual modules (`packages/zigts/`), the build-time tooling (`packages/tools/`), and the `zigttp deploy` client path.

Out of scope: the hosted control plane itself (`api.zigttp.dev`), which has its own reporting channel, and third-party extensions built with `zigttp-sdk`.

## Related Documents

- [docs/threat-model.md](docs/threat-model.md) - threat model for the rule review system and Claude Code integration
- [docs/verification.md](docs/verification.md) - compile-time verification invariants
- [packages/zigts/src/module_binding.zig](packages/zigts/src/module_binding.zig) - capability enforcement surface
