## Why

The compiler still models `!E` as a special failure-row kind and sometimes requires `Row<!E>` to use failures as values. The confirmed language instead treats `E` as an ordinary detached owned type or union everywhere; `!` only labels the Effect failure channel. Keeping both models makes generic propagation, catch, ownership, and diagnostics disagree.

## What Changes

- Replace special `!E` binders and `Row<!E>` value conversions with ordinary type parameters and structural unions.
- Admit every detached owned type in an Effect failure channel; the empty failure type is `never`.
- Define `Without<E, S>` as ordinary union difference and generalize selective catch to one selected type or union.
- Allow recovery to produce `A | B` success without inventing a failure-row value wrapper.
- Rename existing standard-library error values to the confirmed `*Error` style atomically, including `OutOfMemoryError`, without compatibility aliases.

## Capabilities

### Modified Capabilities

- `bootstrap-flow-functions`: use ordinary types for Effect failure channels, propagation, recovery, and row subtraction.
- `bootstrap-type-generics`: remove the separate failure-row kind and solve failure generics through ordinary type and union constraints.
- `bootstrap-ownership`: apply ordinary value ownership and cleanup to failure payloads.
- `bootstrap-silk-stdlib`: expose catch and failure helpers over ordinary types and use canonical error names.

## Impact

Depends on `enforce-return-contract-soundness`. This is a breaking internal and source migration across parsing facts, type normalization, specialization, HIR/MIR outcomes, the standard library, diagnostics, tests, and documentation. No legacy row wrapper or error-name alias remains.
