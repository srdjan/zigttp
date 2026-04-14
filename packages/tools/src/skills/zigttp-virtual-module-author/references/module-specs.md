# Module Spec Artifact

Built-in virtual modules use a separate authoring artifact in:

`packages/zigts/module-specs/<module>.json`

## Purpose

The spec is the canonical design record for:
- module specifier
- required capabilities
- export surface
- effect classes
- failure model
- contract extraction rules
- laws

The Zig module still provides the executable binding and implementation.

## v1 shape

```json
{
  "schemaVersion": 1,
  "specifier": "zigttp:env",
  "source": "packages/zigts/src/modules/env.zig",
  "requiredCapabilities": ["env", "policy_check"],
  "exports": [
    {
      "name": "env",
      "effect": "read",
      "returns": "optional_string",
      "failureSeverity": "expected",
      "contractExtractions": ["env"],
      "laws": ["pure"]
    }
  ]
}
```

## Naming

- `env.zig` -> `env.json`
- `http_mod.zig` -> `http-mod.json`

Use kebab-case for spec filenames.

## Editing rule

When changing a built-in module, update the spec first or in the same patch. Do not leave the binding and spec drifting unless the user explicitly asks for an intermediate migration step.
