## Why

Resolved calls with preserved arguments still do not establish that callers satisfy their targets. Checking the first argument-to-parameter contract makes calls semantically meaningful while the language still has only one built-in type.

## What Changes

- Publish one ordered argument fact for every concrete call argument.
- Map arguments positionally to the parameters of a uniquely resolved target declaration.
- Check argument count and each available argument type against the corresponding declared parameter type.
- Produce a deterministic semantic diagnostic for arity mismatches without duplicating parser or name-resolution diagnostics.
- Publish call-contract compatibility separately from the existing return-type compatibility; with only `I32`, available argument and parameter types are equal by construction.
- Add inspector presets for compatible, too-few, too-many, type-unavailable, and unresolved calls.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-semantic-facts`: Add positional argument binding and the first call-contract compatibility facts and diagnostics.
- `bootstrap-syntax-inspector`: Show argument facts, parameter targets, compatibility, and mismatch diagnostics.

## Impact

This changes semantic fact and diagnostic public APIs, semantic tests and fixtures, compiler documentation, release-candidate validation, and the hidden inspector. The check remains exact and monomorphic for bootstrap `I32`; overloads, conversions, generics, defaults, labels, variadics, and inference remain out of scope.
