---
name: zigttp-virtual-module-author
description: Design and update built-in zigttp virtual modules with explicit capability discipline, module specs, contract metadata, and law review. Use for changes under packages/zigts/src/modules/ and packages/zigts/module-specs/.
---

# zigttp-virtual-module-author

## When To Use

Use this skill when:
- adding a new built-in virtual module
- changing exports or semantics of an existing built-in module
- editing files under `packages/zigts/src/modules/`
- editing files under `packages/zigts/module-specs/`
- reviewing capability declarations, effect classes, or algebraic laws

This skill is for built-in modules first. Treat third-party extensions as out of scope unless the user asks for them explicitly.

## Core Rule

Semantic meaning is declared. Low-level capability reach is enforced.

Do not try to infer semantic claims like `read`, `write`, `pure`, `idempotent_call`, or `inverse_of` from arbitrary Zig implementation details. Those stay explicit in the module spec and binding metadata.

What must be enforced mechanically is the capability boundary:
- module code only touches the outside world through approved helpers
- declared capabilities match helper usage
- raw escape hatches are rejected

## Workflow

1. Read the current Zig module file and its module spec.
2. If the spec is missing, create it first in `packages/zigts/module-specs/`.
3. Lock the semantics before editing code:
   - exported functions
   - required capabilities
   - effect class per export
   - return and failure model
   - contract extraction rules
   - law claims and why they are sound
4. Implement or update the Zig binding and implementation.
5. Keep effects at the edge. Prefer checked capability helpers over direct stdlib or runtime access.
6. Run `zigts expert verify-modules <file> --json` on touched module/spec files.
7. Run targeted Zig tests for the touched module plus the capability audit when relevant.

## Required Outputs

For each module change, keep these aligned:
- `packages/zigts/src/modules/<name>.zig`
- `packages/zigts/module-specs/<name>.json`

The spec is the authoring artifact. The Zig binding is the executable registration layer.

## Law Review

Be conservative with laws.

Only keep a law when it is unconditional and easy to defend. If the law needs timing assumptions, hidden state assumptions, ordering assumptions, or argument preconditions not captured in the law itself, do not declare it.

Read these references as needed:
- capability rules: `references/capability-discipline.md`
- spec format: `references/module-specs.md`
- law checklist: `references/law-review.md`
