# Plan 015: Enforce typed effect and approval boundaries for Pi tools

> **Executor instructions**: Follow this plan step by step. Run every verification command. Touch only the in-scope files. Stop on a listed STOP condition instead of adding another tool-name special case. Update this plan's row in `plans/README.md` when complete.
>
> **Drift check, run first**: `git diff --name-only b00eae29 -- packages/pi/src/registry packages/pi/src/app.zig packages/pi/src/loop.zig packages/pi/src/rpc_mode.zig packages/pi/src/tools/pi_apply_feature_plan.zig packages/pi/src/expert_persona.zig docs/internals/zts-expert-contract.md`

## Status

- **Priority**: P0
- **Effort**: M
- **Risk**: MED
- **Depends on**: none
- **Category**: agent safety / authorization
- **Planned at**: `b00eae29` on 2026-07-10

## Why this matters

Pi's documented invariant is that every workspace edit passes the compiler veto and the configured approval policy before bytes land. The generic tool registry currently has no effect metadata, while `pi_apply_feature_plan` is a normal model-facing tool that writes directly after compiler simulation. The turn loop only recognizes the special `apply_edit` event, and RPC `tools.invoke` calls the registry directly. A model or RPC client can therefore write a compiler-clean candidate while `--no-edit` or an approval callback would reject the same change.

Compiler validation is not authorization: it proves the candidate is acceptable to ZigTS, not that the user approved this file and these exact bytes.

## Current evidence

- `packages/pi/src/app.zig:91-133` registers every full-mode tool in one registry, including `pi_apply_feature_plan` at line 121.
- `packages/pi/src/tools/pi_apply_feature_plan.zig:56-79` resolves the path, reruns `edit_simulate`, and calls `zts.file_io.writeFile` directly.
- `packages/pi/src/loop.zig:518-557,719-723` special-cases only the `apply_edit` event name; ordinary tool calls go to `invokeToolRecovering`.
- `packages/pi/src/loop.zig:780-846` contains the actual approval preview and gate, but the feature-plan tool never reaches it.
- `packages/pi/src/rpc_mode.zig:466-520` exposes direct `registry.invokeJson` with no invocation policy.
- `packages/pi/src/registry/tool.zig:117-124` describes tools by strings and function pointers only; no effect or required capability is represented.
- `docs/internals/zts-expert-contract.md:113-137` says the tools run inside the vetoed loop and RPC analyzer tools cannot apply an edit out of band.

## Scope

In scope:

- `packages/pi/src/registry/tool.zig`
- `packages/pi/src/registry/registry.zig`
- all registered `packages/pi/src/tools/*.zig` definitions needed to add mandatory effect metadata
- `packages/pi/src/app.zig`
- `packages/pi/src/loop.zig`
- `packages/pi/src/rpc_mode.zig`
- `packages/pi/src/expert_persona.zig`
- `docs/internals/zts-expert-contract.md`
- Pi registry, loop, and RPC tests

Out of scope:

- weakening or bypassing the compiler veto;
- a general OS sandbox;
- provider API changes;
- allowing RPC clients to mint approval without an authenticated host capability.

## Steps

### 1. Make tool effects explicit and exhaustive

Add a mandatory ADT to `ToolDef`, for example:

```zig
pub const ToolEffect = enum {
    read_workspace,
    analyze,
    execute_process,
    persist_agent_state,
    write_workspace,
};
```

Do not give the field a permissive default. Annotate every registered tool after inspecting its implementation. Keep the classification about the tool's strongest effect; a process-running tool is not `analyze` merely because its intent is diagnostic.

Add a pure, exhaustively switched policy function over invocation surface (`model_tool`, `rpc_tools_invoke`, and any direct human surface) and `ToolEffect`. Registry lookup may remain generic, but every model/RPC invocation must pass through the policy before `execute` runs.

### 2. Establish one workspace-write path

Remove `pi_apply_feature_plan` from model/RPC registration and delete it if no human-only caller remains. `pi_feature_plan` already produces a candidate; the model must submit that candidate through the existing `apply_edit` state-machine event so `applyVerifiedEdit` owns preview, approval, write, post-apply checks, and receipt generation.

Do not duplicate the approval callback inside a registry tool. One write path is easier to audit and keeps stale-preview/current-file checks in one place.

### 3. Deny mutation at generic invocation surfaces

- RPC `tools.list` should either omit denied tools or include an explicit non-invocable effect field; choose one documented behavior and test it.
- RPC `tools.invoke` must reject `write_workspace` before decoding/executing arguments.
- The model tool batch path must reject `write_workspace` with guidance to emit `apply_edit`.
- Approval policies such as auto-reject/`--no-edit` must remain authoritative.

### 4. Add regression coverage and align the contract

Add tests proving:

- an auto-reject run cannot write by calling `pi_apply_feature_plan` or any tool classified `write_workspace`;
- direct RPC invocation cannot mutate a workspace file;
- a normal `apply_edit` still reaches preview, approval, veto, post-apply checks, and verified-patch receipt;
- all registered tools have an explicit valid effect;
- docs and `tools.list` describe the live behavior.

Update `expert_persona.zig` and `zts-expert-contract.md` so they name `apply_edit` as the sole model-mediated workspace write path.

## Verification

```sh
zig fmt --check build.zig packages/
zig build test-expert-app test-expert test-cassette test-zts-cli test-expert-golden
bash scripts/verify.sh
git diff --check
git status --short
```

## Done criteria

- [x] Every registered tool carries mandatory, reviewed effect metadata.
- [x] Generic model and RPC invocation cannot execute a workspace-writing tool.
- [x] `apply_edit` is the only model-mediated source-write path.
- [x] Auto-reject and direct-RPC regression tests prove no bytes are written.
- [x] Documentation matches the enforced policy.
- [x] All verification commands pass.

## STOP conditions

- A second registered tool is found to mutate workspace source through an indirect helper and cannot be cleanly classified without expanding scope.
- A supported external RPC client currently depends on direct workspace writes; report the compatibility contract before changing it.
- The fix requires weakening the compiler veto or accepting an unbound approval token.
- A focused gate fails twice after reasonable correction.
