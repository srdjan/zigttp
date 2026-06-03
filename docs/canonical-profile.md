# Canonical ZigTS Profile

The canonical profile gives common operations one spelling. `zigttp check` and
`zigttp verify-paths` enforce these rules as ZTS6xx diagnostics. Use
`zigttp normalize <file> --write` for rules that can be rewritten safely, and
`zigttp describe-rule <code>` for the live rule record.

## Rules

| Code | Rule | Canonical form |
|---|---|---|
| ZTS604 | Avoidable `let` | Use `const` unless the binding is reassigned. |
| ZTS605 | Dynamic computed property access | Use literal property names or explicit maps. |
| ZTS608 | Reused arrow helper | Give reusable helpers named function declarations. |
| ZTS609 | Exported function-valued `const` | Export a function declaration. |
| ZTS610 | Public helper effects | Declare helper proof/effect capsules when required. |
| ZTS611 | Proof capsules | Keep proof-carrying helper annotations explicit. |
| ZTS612 | Ternary expression | Use `if`/`else` or `match`. |
| ZTS613 | Compound assignment | Write the full assignment. |
| ZTS614 | Non-leading object spread | Put spread first or write explicit fields. |
| ZTS615 | Complex template interpolation | Bind the value first, then interpolate the binding. |
| ZTS616 | Call-site spread | Pass explicit arguments. |
| ZTS617 | Default parameter value | Use an explicit body-level default. |
| ZTS618 | Nested destructuring | Destructure one level at a time. |
| ZTS619 | Unused index alias in `for...of` | Iterate the array directly. |
| ZTS620 | Boolean compared to boolean literal | Use the boolean expression or negation directly. |

## Commands

```bash
zigttp check --json examples/handler/handler.ts
zigttp normalize src/handler.ts --check
zigttp normalize src/handler.ts --write
zigttp describe-rule ZTS604 --json
zigttp describe-rule --hash
```

`normalize --check` exits non-zero when the file is not already canonical. It
is the right CI gate when a project wants canonical form enforced before review.

## Proof Interaction

Canonical code reduces the number of equivalent shapes the analyzer and expert
agent need to handle. A handler with no ZTS6xx diagnostics carries the
`canonical` proof property, and `Response & Spec<"canonical">` can discharge
against it.
