## ADDED Requirements

### Requirement: Module surfaces encode public foreign functions by symbol and contract

A public foreign function SHALL appear in its module's semantic surface with its name, unsafe
contract, ABI, native symbol, and Silk parameter and result types, and SHALL NOT encode any
target-dependent classification. Changing the native symbol or any signature type SHALL change the
surface and invalidate direct dependents; re-spelling the declaration without changing those facts
SHALL leave the surface equal.

#### Scenario: Symbol rename invalidates dependents

- **WHEN** module `a` changes `pub unsafe extern "C" fn cAbs(value: i32) -> i32 as "abs"` to `as "labs"`
- **THEN** the surface of `a` differs and every direct importer of `cAbs` is invalidated

#### Scenario: Whitespace change leaves the surface equal

- **WHEN** only trivia inside the foreign declaration changes
- **THEN** the encoded surface is byte-identical and no dependent is invalidated
