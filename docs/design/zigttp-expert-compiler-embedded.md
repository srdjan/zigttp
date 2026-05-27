# zigttp Expert Compiler-Embedded Assistant

This document defines the strategy for the `zigttp expert` agent as a
compiler-embedded assistant. It is a design note, not an implementation report.
The current implementation embeds the agent persona, tool registry, compiler
metadata, canonical diagnostics, compiler-authored refactor previews, and edit
veto into the `zigttp` and `zigts` binaries. The one-way language profile
described here is the enforced standard for generated and checked handler code.

**Implementation status (2026-05-27).** The canonical profile is enforced
unconditionally on every check. The strict checker carries eleven canonical
rules (`ZTS608`-`ZTS618`); the agent persona embeds the
[`canonical-style`](../../packages/pi/src/skills/canonical-style.md) skill on
every prompt assembly. The full rule reference with before/after pairs and
migration recipes lives at [`canonical-profile.md`](../canonical-profile.md).
The opt-in `--one-way` flag described in Phase 3 below was not built; the
rules turned out to make sense unconditionally and a mode selector would have
been overhead without users.

## Goal

`zigttp expert` should produce handler code that is easy for the compiler to
prove and easy for a human to review. The agent should have fewer valid choices
than a general TypeScript assistant, and every edit should pass through the
same compiler checks that protect normal builds.

The operating model is narrow:

- Generate only the ZigTS subset that zigttp can parse, type-check, verify, and
  compile with low overhead.
- Prefer one canonical spelling for each operation.
- Let the compiler provide facts: allowed syntax, modules, diagnostics,
  effects, proof properties, and policy hashes.
- Treat the model as advisory. The compiler accepts or rejects edits.

This matches zigttp's product constraints: fast cold starts, small binaries,
bounded memory, request isolation, and FaaS-friendly handler execution.

## One-Way ZigTS Profile

ZigTS is already smaller than TypeScript. The one-way profile is stricter: it
chooses a canonical idiom when the language currently allows more than one.
Compiler enforcement is part of the normal strict profile rather than a
separate opt-in mode.

### Canonical Forms

Use these forms in generated code:

| Task | Canonical form |
|---|---|
| Handler and helper declarations | Named `function` declarations with explicit parameter and return types |
| Local callbacks | Arrow functions only when passed directly as values |
| Bindings | `const` by default; `let` only when reassigned |
| Absent values | `undefined` only |
| Branching | `if` / `else` for guards, `match` for closed alternatives |
| Iteration | `for (const item of items)` over finite collections |
| Errors | `Result` values and explicit `.ok` checks |
| External effects | `Effects<T, "...">` where a public helper has a capability ceiling |
| Proof obligations | `Spec<...>` on handlers and `Proof<T, "...">` on helpers when needed |
| Module imports | Named imports from literal `zigttp:*` or registered `zigttp-ext:*` specifiers |
| Capability keys | String literals or compiler-visible const literal aliases |

The profile should avoid duplicate expression styles in generated code. For
example, the agent should not alternate between a named helper and an inline
function expression when both perform the same role. Stable style matters
because generated patches become smaller, review is easier, and compiler repair
plans can target predictable syntax.

### Rejected Forms

These are already rejected or discouraged by ZigTS and should never appear in
agent output:

- `any`, `as`, and `satisfies`
- `class`, `this`, `new`, and `super`
- `try`, `catch`, `finally`, and `throw`
- `switch`, C-style `for`, `while`, `do...while`, and `for...in`
- `null`, `==`, `!=`, `++`, `--`, `var`, decorators, namespaces, and enums
- Default imports, namespace imports, side-effect imports, re-exports, and
  `export default`
- Dynamic computed property access when the key is not compiler-visible
- Dynamic environment, cache, SQL, egress, or capability keys
- Avoidable `let`

Some of these are hard errors in the parser, stripper, or strict checker.
Canonical authoring rules that need semantic context are emitted as default
ZTS6xx diagnostics.

### Deliberate TypeScript Cuts

The profile should not grow toward full TypeScript. Complex generics,
overloaded APIs, declaration merging, conditional types, ambient namespaces,
and assertion-heavy code all increase the number of valid-looking programs the
model can produce while giving the Zig runtime no benefit.

Allowed generic syntax should stay small and tied to existing proof carriers:
generic type aliases, function generics where the checker can resolve them, and
the built-in `Spec`, `Proof`, and `Effects` capsules. A helper that needs more
type machinery than that should usually be split into simpler functions with
explicit inputs and outputs.

## Determinism And Agent Reliability

General TypeScript gives an assistant many ways to express the same program.
That hurts reliability. The model may choose a feature the runtime does not
support, mix incompatible idioms, or repair one diagnostic by introducing a
different one.

ZigTS improves the odds by reducing the grammar:

- Fewer syntax branches means fewer invalid generations.
- No exceptions means failure paths stay in ordinary control flow.
- No classes or `this` means dependencies are explicit function parameters.
- No unbounded loops means verification can reason over the IR tree without a
  general fixpoint analysis.
- One absent sentinel keeps optional narrowing predictable.
- Literal capability keys let contracts list environment variables, cache
  namespaces, SQL queries, and egress hosts.

The agent benefits because the compiler can return concrete diagnostics and
suggestions. A smaller language also makes examples more valuable: every
example covers a larger fraction of the legal output space.

Determinism also comes from the toolchain. The expert prompt is assembled from
compiled-in text plus live compiler snapshots. `zigts meta` exposes the
compiler version, policy version, policy hash, and module registry hash. When a
session resumes under a different policy hash, the agent can treat previous
rule citations as stale and ask the running binary for the current facts.

The important boundary is this: the model does not define correctness. It
proposes code. The compiler checks the proposal.

## Compiler-Embedded Architecture

The current architecture already has the right shape. The assistant is
embedded in the toolchain by compiling the persona, tools, policies, and veto
path into the binary.

### Current Components

- `packages/pi/src/expert_persona.zig` builds the system prompt from a fixed
  prologue, the embedded `zigts-expert` skill, live rule and module snapshots,
  and optional read-only project context.
- `packages/pi/src/app.zig` registers compiler-native tools for metadata,
  language features, virtual modules, rule search, path verification, edit
  simulation, effect rows, property ratchets, witness inspection, and build
  steps.
- `packages/pi/src/veto.zig` runs `edit_simulate` before any proposed edit can
  land. Pre-existing violations do not block the edit; new violations do.
- `packages/tools/src/expert_meta.zig` exposes policy and module hashes so
  sessions and CI can pin the rule set they used.
- `docs/internals/zigts-expert-contract.md` defines the stable JSON contract
  consumed by the agent loop and external clients.

The model provider can be remote. The compiler-embedded guarantee is not that
the model weights live inside the binary. The guarantee is that the model sees
compiled-in instructions and can only apply code through compiler-owned tools.

### Compile-Time Assistance

The assistant should help during compilation in three ways.

First, it should explain diagnostics with exact rule data. `zigts_expert_search`
and `zigts_expert_describe_rule` already provide this path. The agent should
prefer those tools over memory whenever the user asks about syntax, policy, or
error codes.

Second, it should propose canonical refactors. `zigts canonicalize <file>
--json [--simulate]` produces compiler-authored repair intents for local safe
rewrites such as reused function-expression helpers to named helpers,
avoidable `let` to `const`, and literal capability aliases that become
compiler-visible after the same `let` to `const` rewrite. With `--simulate`,
the compiler applies previews in memory and runs the same edit simulation veto
used by the agent loop. The agent can present the refactor, but the compiler
owns the proposed transformation shape and edits still pass through the veto
path.

Third, it should guide optimization without making hidden runtime trade-offs.
The relevant facts already exist in the proof system: effect rows, proven
properties, path witnesses, route contracts, and module capability metadata.
Optimization suggestions should preserve or improve the proof card. Any change
that widens an effect row, loses determinism, or adds egress belongs in the
user-visible proof summary.

### FaaS Constraints

The assistant must not push complexity into the runtime. Runtime handlers still
need:

- Straight-line execution without Promises or a JS event loop.
- Bounded request memory and isolated runtime contexts.
- Explicit capability use visible to contract extraction.
- Small generated code that compiles quickly and does not increase cold-start
  size with helper frameworks.
- Deterministic replay and path generation for tests and witnesses.

The agent may be sophisticated. The generated handler should stay simple.

## Implementation Roadmap

### Phase 1: Documentation Baseline

Create this design document and link it from the documentation index. Keep the
scope clear: the one-way profile is the agent's target style and the default
compiler contract.

Acceptance criteria:

- The docs explain current behavior and roadmap behavior separately.
- The docs link to the language subset, feature detection, TypeScript,
  verification, architecture, `pi`, and expert contract references.
- No compiler behavior changes are required.

### Phase 2: Prompt And Skill Alignment

Update the embedded `zigts-expert` skill and persona language so generated code
defaults to the one-way profile.

Required changes:

- Add a short canonical-style section to the embedded skill.
- Include examples of invalid general TypeScript and canonical ZigTS
  replacements.
- Keep the existing instruction that language and module questions must call
  live compiler tools.
- Keep the prompt under the compiled prompt cap and preserve project-context
  truncation behavior.

Validation:

- Unit tests for prompt assembly still pass.
- The prompt still names every registered tool that the prologue expects.
- `zigts expert --print` can answer a language question by consulting the live
  feature/module tools.

### Phase 3: Default Canonical Profile

Extend `strict_checker.zig` with default-on canonical diagnostics rather than
adding a parallel parser.

Candidate diagnostics:

- Canonical helper declaration required for exported or handler-reachable
  helpers.
- Inline arrow helper should become a named function when reused.
- Public helper should carry explicit `Effects` / `Proof` capsules when it
  participates in capability or declared-spec paths.
- Non-literal capability keys must be rewritten to literals or const literal
  aliases.
- Avoidable `let` remains an error in strict mode.

Canonical diagnostics are part of the normal `zigts check` and
`zigts verify-paths` surfaces. They fail builds like the rest of the strict
profile.

### Phase 4: Compiler-Authored Refactors

Add structured refactor intents that the expert can apply through the existing
veto flow.

The shape should be deterministic:

```json
{
  "kind": "canonicalize_let",
  "file": "handler.ts",
  "line": 12,
  "replacement": "const region = env(\"REGION\") ?? \"iad\";"
}
```

The compiler should only emit intents when the rewrite is local and semantics
preserving. Larger transformations should remain suggestions until the proof
pipeline can validate them mechanically.

Validation:

- Run `edit_simulate` against every generated refactor.
- Compare pre/post proof properties.
- Reject edits that introduce new diagnostics or widen effect rows without
  user approval.

### Phase 5: Optimization And Ratchet Loop

Use existing proof tools to make optimization suggestions safe:

- `zigts_expert_effects` shows capability and determinism deltas.
- `zigts_expert_ratchet` shows proven properties that can become declared
  specs.
- `zigts_expert_narrow` exposes path constraints behind flow failures.
- `pi_repair_plan` and `pi_apply_repair_plan` keep property repairs tied to
  compiler facts.

The agent should recommend optimizations only when it can state the proof-card
impact. Examples: a deterministic read-only handler can use proof caching; a
route guard can avoid entering JS for non-matching requests; a literal env key
can move startup validation into the contract.

### Phase 6: Promotion Gates

Keep promotion mechanical:

- All examples and fixtures pass the default canonical profile.
- `zig build test-zigts` and `zig build test` pass.
- The expert can repair common canonical-profile violations without manual
  edits.
- The JSON contract either remains v1-compatible or receives a documented v2
  bump.
- The release notes state which diagnostics moved from suggestion to warning
  or error.

## Testing Strategy

For the documentation phase:

```bash
git diff --check
zig build test-zigts
```

For future implementation phases:

```bash
zig build test
zig build test-zigts
zigts check --json examples/handler/handler.ts
zigts verify-paths --json examples/handler/handler.ts
zigts meta --json
```

Add fixture tests for every canonical diagnostic and every compiler-authored
refactor. Each fixture should include:

- Source before the repair.
- Expected diagnostic code and suggestion.
- Proposed replacement when the compiler can prove it locally.
- Post-repair `edit_simulate` result.
- Proof-property delta when contract extraction reaches that phase.

## References

- [Language Subset](../language-subset.md)
- [TypeScript Support](../typescript.md)
- [Feature Detection Matrix](../feature-detection.md)
- [Compile-Time Handler Verification](../verification.md)
- [Architecture](../internals/architecture.md)
- [zigts Expert Contract](../internals/zigts-expert-contract.md)
- [`pi` package README](../../packages/pi/README.md)
