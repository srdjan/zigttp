# Law Review Checklist

Only declare a law when it is unconditional.

## Keep a law only if all answers are yes

### `pure`

- Is the result a function of the arguments only?
- Is there no hidden dependency on clock, random, runtime state, env, cache, IO, or mutable global state?
- Would two adjacent calls with the same structural arguments always be observationally equivalent?

### `idempotent_call`

- Does repeating the same call leave the external state observably unchanged after the first call?
- Is this true without relying on call ordering around other effects?
- Is the write effect still correctly classified as `write`?

### `inverse_of`

- Is the inverse relation true in both directions for the declared pair?
- Are canonicalization losses impossible?
- Would round-tripping hold for every supported input, not just common cases?

### `absorbing`

- Is the matched argument shape exact and explicit?
- Is the residue fixed for every execution, regardless of time or state?

## Default

If there is any doubt, remove the law and keep the weaker contract.
