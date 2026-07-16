---
title: Sub-handler contract extraction must not inherit the strict profile
date: 2026-07-16
category: security-issues
module: runtime system dispatch and handler precompilation
problem_type: security_issue
component: tooling
symptoms:
  - System sub-handler runtime tests fail after contract extraction is added
  - Existing non-canonical fixtures are rejected by ZTS6xx diagnostics
root_cause: config_error
resolution_type: code_fix
severity: high
tags: [handler-contract, policy-derivation, strict-profile, fail-closed]
---

# Sub-handler contract extraction must not inherit the strict profile

## Problem

System sub-handlers need their own contract-derived egress and environment policy. Routing their sources through the general precompiler to extract contracts accidentally imposed the default-on strict ZigTS profile on handlers that had never been subject to that gate.

## Symptoms

- Twenty runtime tests failed on ZTS6xx canonical-profile diagnostics after per-target contract extraction was introduced.
- The failures appeared before pool construction, even though the requested security change was policy derivation rather than stricter source acceptance.

## What Didn't Work

- Calling `compileHandler` with only `.emit_contract = true` was not behavior-preserving. `ResolveOptions.strict` defaults to `true` in `packages/zigts/src/pipeline.zig`, so an omitted option enables the strict checker.
- Disabling the boolean or type checker would have weakened real soundness gates. Their failures remain fatal in `packages/tools/src/precompile.zig`.

## Solution

Expose the existing resolver choice through a default-preserving compile option:

```zig
pub const CompileOptions = struct {
    emit_contract: bool = false,
    strict: bool = true,
    // ...
};

var resolved = try zigts.pipeline.resolve(
    allocator,
    parsed,
    .{ .type_env = type_env_storage.envPtr(), .service_type_context = stc_ptr, .strict = opts.strict },
);
```

Only system sub-handler contract extraction opts out:

```zig
var compiled = try precompile.compileHandler(self.allocator, source, entry, .{
    .emit_contract = true,
    .strict = false,
});
```

The `true` default preserves every existing caller. The sub-handler path still fails closed if parsing, boolean checking, type checking, contract extraction, or pool initialization fails; it skips only the canonical-profile diagnostics that were not previously part of that runtime path.

## Why This Works

Strict-profile enforcement and contract extraction are separate concerns. Contract extraction supplies the target-specific allowlist used by `contractToRuntimePolicy`, while `.strict = false` prevents that extraction step from silently expanding the source-language contract. Boolean and type diagnostics are still evaluated and still return `error.SoundModeViolation` on errors.

## Prevention

- When reusing a general compilation pipeline for metadata extraction, audit every default-on validation gate and explicitly preserve the old caller's acceptance contract.
- Keep opt-outs narrow and default-preserving: add a defaulted option at the shared boundary and override it only at the compatibility-sensitive call site.
- Verify with fixtures that distinguish strict-profile diagnostics from boolean/type soundness failures; never make a broad checker opt-out to clear unrelated fixtures.

## Related Issues

- `packages/runtime/src/in_process_dispatch.zig`
- `packages/tools/src/precompile.zig`
- `packages/zigts/src/pipeline.zig`
