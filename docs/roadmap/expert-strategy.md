# Expert strategy: compiler-embedded AI for zigttp

This document names the closed loop that powers `zigttp expert`, audits what is shipped against it, and lays out the north-star roadmap. It is the canonical entry point for contributors and external readers who want to understand why zigttp narrows TypeScript the way it does and how the compiler and the AI agent reinforce each other.

The document layers a thesis section (audience: prospective users, external developers) over a technical reference (audience: contributors). Both audiences should read both halves; the thesis explains why the architecture has its shape, and the reference shows where to find the code.

---

## 1. Context and thesis

### The closed loop

zigttp is built around a three-leg loop:

1. **A narrowed language.** ZigTS is a strict TypeScript subset: one canonical spelling per operation, no ambient effects, no constructs whose meaning is run-time-dependent.
2. **A tight compiler veto.** The pipeline (`packages/zigts/src/pipeline.zig`) extracts a proof card per handler. Edits that would weaken any proven property are rejected before they reach disk.
3. **A compiler-embedded agent.** `zigttp expert` does not run as an external assistant that emits guesses. It runs in-process, sees the live rule registry, and proposes edits only through veto-gated tools.

Each leg is load-bearing. Remove the language constraint and the proofs become expensive or impossible. Remove the veto and the agent's mis-generations land on disk. Remove the agent and the language is a discipline that only the most patient developers will follow. The product thesis is that the three together change the cost curve of generated code: the generator's failure modes shrink to what the compiler can already reject.

### Why this beats "fix it in prompt"

Prompt-only approaches to AI reliability bound the model's behavior with text. They scale poorly because the legal-program space the model can emit is whatever the language permits, which for full TypeScript is enormous. zigttp narrows the legal space at the language level. Whatever the model emits is then filtered by a static analyzer that already exists for non-AI reasons (build-time verification, contract extraction, deterministic replay). The AI is a consumer of an analyzer that was going to be built anyway, not a system whose correctness rests on prompt engineering.

### Honest framing

This architecture reduces generation errors. It does not eliminate them. The veto loop catches structural and proof-level violations. It does not catch errors that are legal under every rule but wrong for the user's intent. The mitigation there is the same as for any code: tests, witnesses, review. The claim is narrower: *errors that are statically detectable are statically rejected, and the legal surface in which the model operates is small enough that statically detectable errors cover a large fraction of the realistic failure space.*

---

## 2. The canonical ZigTS subset

ZigTS removes most of TypeScript that does not carry its weight at runtime, and the canonical profile narrows the rest until each operation has exactly one canonical spelling. Reference material:

- [docs/language-subset.md](../language-subset.md) — supported runtime syntax and supported TypeScript surface.
- [docs/canonical-profile.md](../canonical-profile.md) — full ZTS6xx rule reference with migration recipes.
- [docs/feature-detection.md](../feature-detection.md) — the 54 parser features and 3 stripper features that are detected and rejected at parse time.
- [docs/restrictions-to-proofs.md](../restrictions-to-proofs.md) — the mapping from each language cut to the failure class it eliminates and the proof it unlocks.

The profile is enforced unconditionally on every `zigttp check` and `zigttp verify-paths` run. There is no opt-in flag.

### Canonical rules at a glance

| Code   | What is banned                              | Canonical form                              | Proof / property unlocked                                      |
| ------ | ------------------------------------------- | ------------------------------------------- | -------------------------------------------------------------- |
| ZTS604 | `let` for never-reassigned binding          | `const`                                     | Data-flow treats binding as a value; strengthens purity proofs |
| ZTS605 | Dynamic computed property access            | Static-literal or `const`-bound key         | Static object shape; sound type narrowing                      |
| ZTS608 | Arrow as reused helper                      | Named `function` declaration                | Stack traces, tool referencing, "reused" signal                |
| ZTS609 | `export const f = (...) => ...`             | `export function f(...)`                    | Same as ZTS608, at module boundary                             |
| ZTS610 | Public helper with undeclared effects       | `Effects<T, S>` capsule                     | Capability propagation; sandbox enforcement                    |
| ZTS611 | Public helper without declared proofs       | `Spec<T>` capsule                           | Compiler-discharged spec assertions                            |
| ZTS612 | Ternary expression `a ? b : c`              | `if`/`else` or `match`                      | One control-flow shape; flow checker simplification            |
| ZTS613 | Compound assignment (`+=`, etc.)            | Explicit `x = x + e`                        | Mutation site is named, not hidden in operator                 |
| ZTS614 | Non-leading object spread `{x: 1, ...base}` | Leading spread `{...base, x: 1}`            | Predictable property precedence                                |
| ZTS615 | Complex template interpolation `${getX()}`  | Hoist to `const` above the template         | Side-effect-free template literals                             |
| ZTS616 | Call-site spread `f(...args)`               | Positional args or widened signature        | Static arity; monomorphic call inlining                        |
| ZTS617 | Default parameter value `(a = v)`           | Accept `T \| undefined` and resolve in body | Default-value site is named, not hidden in signature           |
| ZTS618 | Nested destructuring `{a: {b}}`             | One level then follow-up `const`            | Bounded destructure depth; readable error sites                |

ZTS612 through ZTS618 are the most recent slice (commit `c0b66fa`, May 2026). They round out the canonical profile by removing the last expression-level alternates whose presence forced the strict checker to carry two-arm logic per rule.

The numbers ZTS600 through ZTS603 cover the foundational profile rules (implicit `unknown`, missing public annotation, dynamic capability access, non-exhaustive profile match) and are documented in `packages/zigts/src/strict_checker.zig`.

### One way to do things

The motivating principle is that semantically equivalent expressions should have one canonical spelling, and the compiler should reject the alternates. The table above is the current equivalence-class audit. Each row was a choice between two or more spellings; the canonical column is the survivor. The compiler-authored refactor in `packages/zigts/src/canonicalize.zig` can mechanically convert most banned forms to their canonical equivalent, and `zigttp expert` invokes it through the `canonicalize` tool.

### Explicit non-goals

Narrowing is not the same as primitivism. zigttp deliberately keeps:

- **Generic type parameters**, including on functions and type aliases. They cost nothing at runtime (stripped) and pay back in API expressiveness.
- **`distinct type` declarations** for branded primitives. They are the cheapest way to make injection bugs impossible at the type level.
- **`Spec<T>`, `Proof<T, S>`, `Effects<T, S>` from `zigttp:types`.** These are the carrier types for compiler-discharged contracts and capability declarations. Without them the proof card has nowhere to attach to in user code.
- **JSX/TSX** when the file extension enables it. JSX is a constrained, well-typed form; it does not add expressive ambiguity.

The line is: features that *narrow* the runtime semantics stay; features that *add alternate spellings* of an existing semantic operation go.

---

## 3. Determinism leads to AI reliability

### The hallucination-surface argument

A code-generating model is constrained by what the language permits. When the language is full TypeScript, every legal program is a candidate output and the model can be wrong in millions of locally-plausible ways. When the language is ZigTS, the candidate space is bounded by the parser, the strict profile, and the contract extractor. The model can still produce wrong code, but the *legal* wrong-code space is much smaller, which means a higher fraction of mis-generations are statically detectable.

This is not a claim about model size or prompt quality. It is a claim about the search space the model is sampling from. Smaller space, smaller absolute number of statically-undetectable wrong programs.

### Three reinforcing mechanisms

zigttp filters mis-generations at three depths, each rejecting a different failure mode:

1. **Parse-time feature detection** (`packages/zigts/src/parser/parse.zig`, `docs/feature-detection.md`). Catches syntactic hallucinations: `while`, `try/catch`, `class`, `new`, `var`, `==`, regex literals, and 47 others. The compiler emits a targeted suggestion (`use for...of`, `use Result type`, etc.) so the agent can recover in one turn.
2. **ZTS6xx canonical veto** (`packages/zigts/src/strict_checker.zig`). Catches stylistic hallucinations: the model picks a legal-but-non-canonical spelling (ternary, compound assignment, arrow helper). The rule fires with the canonical replacement.
3. **Proof-card extraction** (`packages/zigts/src/handler_contract.zig`, `packages/zigts/src/flow_checker.zig`). Catches semantic hallucinations: the edit compiles, parses, passes the strict checker, but weakens a proven property (purity, read-only, idempotency, secret containment). The veto fires at the proof-card diff.

Each layer rejects a different class of error, so a hallucination has to be wrong in *none* of these three ways to land on disk. That is a much stricter filter than any single check.

### The veto loop

The agent does not write files directly. The shipped flow is:

1. The model proposes an edit through `pi_repair_plan` or `pi_feature_plan`.
2. `zigts_expert_edit_simulate` runs the full pipeline (parse, strict check, type check, flow check, contract extract) on the proposed result without touching the working tree.
3. If any check fails, the rejection is returned to the model as a structured diagnostic with rule code, source location, and canonical replacement when available.
4. Only on green does `pi_apply_repair_plan` (or `pi_apply_feature_plan`) write the file. A second veto pass runs against the post-write state as a defense-in-depth check.

This loop is the safety net. The language design is the prior: it makes the safety net's job tractable.

---

## 4. Compiler-embedded architecture

### Entry-point flow

```
zigttp expert
  -> packages/runtime/src/dev_cli.zig            (CLI dispatch)
  -> packages/runtime/src/cli_auth.zig           (injectStoredProvidersIntoEnv: ~/.zigttp/providers.json -> env)
  -> packages/pi/src/app.zig                     (tool registry, REPL/RPC dispatch)
  -> packages/pi/src/agent.zig                   (BackendDescriptor: stub | anthropic | openai)
  -> packages/pi/src/loop.zig                    (turn driver)
  -> packages/pi/src/tools/zigts_expert_*.zig    (in-process tool calls)
```

Provider routing in `agent.zig` reads `ANTHROPIC_API_KEY` and `OPENAI_API_KEY` from the environment. The Anthropic and OpenAI clients are both live as of slice 5b. Shell-set variables take precedence over the file-backed store; the file fills gaps. The provider store is at `~/.zigttp/providers.json` with mode 0600 and is managed by `zigttp auth claude|openai|status|revoke`.

### System prompt

`packages/pi/src/expert_persona.zig` builds the system prompt at session start. The prompt has three parts:

- **Persona prologue.** 46 rules governing agent behavior: inspect before editing, batch tool calls, prefer compiler veto over self-judgement, canonical ZigTS one-way preference. This text never gets truncated.
- **Live rule and feature snapshots.** Injected from `packages/zigts/src/rule_registry.zig` (every ZTS code with name and one-line description) and the feature detection registry. The injection means the prompt always reflects the binary's actual rule set, not stale prose. The `policyHash` is included as an anchor.
- **Project context.** AGENTS.md or equivalent project memory if present. This is the only section that gets truncated when the prompt approaches the 128 KiB cap.

Truncation order: project context first, then rule snapshots compressed to codes-only, then features compressed; persona prologue and rule names are last to be cut.

### Tool registry

The agent operates exclusively through registered tools. Each tool is a Zig function returning `ToolResult { ok: bool, llm_text: []u8 }`. Tools are grouped by phase:

| Phase   | Tools                                                                                                                                                                                                                        | Purpose                                              |
| ------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| Read    | `workspace_list_files`, `workspace_read_file`, `workspace_search_text`, `zigts_expert_features`, `zigts_expert_modules`, `zigts_expert_meta`, `zigts_expert_describe_rule`, `zigts_expert_search`                            | Inspect repository and rule set without side effects |
| Analyze | `zigts_expert_verify_paths`, `zigts_expert_edit_simulate`, `zigts_expert_review_patch`, `zigts_expert_prove_patch`, `zigts_expert_effects`, `zigts_expert_narrow`, `zigts_expert_ratchet`, `pi_witnesses`, `pi_specs_status` | Run the analyzer on existing or hypothetical code    |
| Repair  | `pi_repair_plan`, `pi_apply_repair_plan`, `zigts_expert_canonicalize`, `pi_feature_plan`, `pi_forge_route`, `pi_apply_feature_plan`                                                                                          | Propose, simulate, and (on green) apply edits        |
| Prove   | `zigts_expert_system_proof`, `zigts_expert_verify_modules`, `pi_goal_check`, `zigts_check`                                                                                                                                   | Discharge cross-handler and module-level properties  |
| Build   | `zig_build_step`, `zig_test_step`                                                                                                                                                                                            | Drive the Zig build and test pipeline                |

The invariant is that *every mutation goes through an "apply" tool that runs the analyzer first and rejects the apply if the analyzer rejects.* There is no direct `write_file` primitive exposed to the model.

### Pipeline integration points

The Zig pipeline in `packages/zigts/src/pipeline.zig` advances each file through three stages:

1. **ParsedModule** — IR exists, nothing checked. The `BoolChecker` confirms syntactic well-formedness.
2. **ResolvedModule** — `TypeChecker` and `StrictChecker` have run. Type errors and ZTS6xx violations are present at this point.
3. **CheckedModule** — `HandlerVerifier`, `FlowChecker`, and contract extraction have run. The proof card is complete.

The agent observes these stages read-only. Mutations are deferred to the apply tools, which re-run the full pipeline on the post-edit state. The agent never reaches into the pipeline mid-stage.

The relevant checker implementations:

- `packages/zigts/src/handler_verifier.zig` (1286 lines) — exhaustive returns, result-safety, unreachable code, unused vars.
- `packages/zigts/src/type_checker.zig` (2063 lines) — type inference and annotation checking.
- `packages/zigts/src/strict_checker.zig` (1213 lines) — ZTS6xx canonical profile.
- `packages/zigts/src/flow_checker.zig` (1972 lines) — data flow, control flow, property proof.
- `packages/zigts/src/handler_contract.zig` (1286 lines) — proof-card assembly and bytecode veto gate.

---

## 5. Implementation roadmap

The shipped surface above is substantial. The roadmap below is north-star, in three horizons.

### Near-term: finish the canonical profile and harness gaps

- **Continue the equivalence-class audit.** ZTS612-618 closed the expression-level alternates the original audit identified. Candidates for ZTS619+ are visible in handler code: index aliasing in `for...of` loops, redundant `as const` on already-immutable literals, parameter destructure on call sites where the callee declares the wider shape. Each candidate needs a write-up that names the proof it unlocks, not just the spelling it bans. Don't add rules whose only justification is consistency; every rule should earn its cost in the proof card.
- **Cassette harness for provider tests.** Per the deferred slice 5b note, the OpenAI client landed without a record/replay harness. Without it, provider regression tests either hit the live API (slow, flaky, paid) or stub at too high a level. The harness should record on first run, replay deterministically thereafter, and round-trip the wire format faithfully enough that streaming-vs-batch differences are visible.
- **Streaming for OpenAI.** Anthropic streaming is wired; OpenAI is batch-only. The user-facing latency win is real and the implementation is small once the cassette harness exists.

### Mid-term: structural tool evolution

- **AST-aware tool surface.** Today's apply tools take file regions. The mid-term move is typed AST edit primitives: `rename_symbol`, `extract_function`, `inline_const`, `lift_effect`, `narrow_type`. Each primitive describes the transformation in terms the compiler can verify by structural comparison, not just by re-running the analyzer on the result. The benefit is two-fold: cheaper to verify, and the model can describe its intent in terms that match the rule-set vocabulary (a `narrow_type` operation is exactly what ZTS600 wants).
- **Persistent witness corpus and few-shot context.** `pi_witnesses` already persists falsifying inputs found during goal checking. The mid-term move is to surface a curated subset of recent witnesses in the persona snapshot as few-shot context. The model sees "this input class breaks handlers like this one" and is biased away from the same class. This is a soundness-neutral change; the real veto still runs.
- **Learned veto-prediction.** A small classifier trained on the tool-call trace (features: rule codes that fired in recent simulations, edit shapes, file regions touched) can predict which proposed edits will fail veto. The agent short-circuits obvious rejections without spending a model turn on them. Soundness-neutral: the real veto still runs on whatever the predictor lets through. This is a latency and cost optimization, not a correctness mechanism.
- **Cross-session memory.** A project-scoped `.zigttp/memory.jsonl` separates persona facts (project-wide design decisions, user preferences) from session scratch. The expert reads it at session start and may append to it through a `remember_fact` tool whose entries are reviewed before next-session injection.

### North star: compiler and model co-design

- **System-wide reasoning.** Today the expert reasons per-handler. `system_proof` and `system_linker.zig` already link handlers at the contract level. The north star is multi-handler refactors: the agent proposes an edit spanning two or more handlers, and the compiler proves the system-level contract still holds. This unlocks transformations like "rename this route everywhere it is referenced" as a verified operation, not a textual search-and-replace.
- **Structured repair intents in every diagnostic.** Diagnostics today target humans: prose, source location, sometimes an example. The co-design step is to emit, alongside the prose, a typed `RepairIntent` enum the model consumes directly. A ZTS612 diagnostic emits `RepairIntent.replace_ternary_with_if`; a ZTS617 emits `RepairIntent.lift_default_to_body`. The model picks the corresponding apply primitive without inferring it from prose. This is the cleanest closing of the loop: the compiler does not just reject, it tells the model exactly which repair primitive applies.
- **Performance as proof for the agent itself.** The deferred performance-as-proof initiative (signed, reproducible perf receipts via `zigttp bench`) extends naturally to expert sessions. Every applied edit ships a receipt: "this edit did not regress these properties by more than N% on this corpus." Sessions become auditable, not just functional. The user can see, after the fact, which edits the agent applied and what evidence was attached to each.
- **Public, reproducible benchmark of generation reliability.** Once the three north-star items are in place, the architecture supports a public benchmark: a fixed corpus of feature requests, run under `zigttp expert` against a pinned model, with a single number — fraction of requests that produce a green-veto patch on the first turn — as the headline metric. This is the only number that captures whether the loop works. It should be measured, not estimated.

---

## 6. Verification

This document is a reference, not code, so verification is structural:

- Every cited file path exists in the tree at the time of writing. New readers can `grep` the names and find the code.
- Every ZTS code in the table appears in `packages/zigts/src/rule_registry.zig`. The `policyHash` available via `zigttp describe-rule --hash` anchors the rule set the table reflects.
- Every roadmap item names the concrete tool, file, or memory note it touches. None of them are aspirational without an attachment point in the current code.

Two reading passes are recommended before considering this document settled:

1. **Contributor pass.** Read sections 4 and 5. Can a contributor unfamiliar with the expert surface find the entry points, understand the tool registry, and locate the work for any roadmap item? If a name in the doc does not resolve to a file or function, the doc is wrong, not the code.
2. **External-developer pass.** Read sections 1, 2, and 3. Does the thesis explain the loop without leaning on insider jargon? Does the canonical-profile table give a prospective user a clear sense of what writing zigttp code feels like? If a paragraph requires reading `packages/zigts/src/*.zig` to make sense, it belongs in section 4, not earlier.

Future slices that touch the expert surface should update this document in the same commit, not after the fact. The document is meant to stay current; that is its only mechanism for staying useful.
