## ADDED Requirements

### Requirement: Module surfaces encode inherent members

A module's semantic surface SHALL include every inherent impl head and every inherent member with
its owner, name, receiver classification, complete type-parameter list, parameter contract,
result, and failure and requirement rows, under the same header-only rule every other declaration
kind follows. Adding, removing, or changing the contract of an inherent member SHALL invalidate
direct dependents; editing a member body SHALL leave the surface equal.

#### Scenario: Invalidate on a member contract change

- **WHEN** a dependency changes `impl Counter { pub fn value(self: &Self) -> i32 }` to return `i64`
- **THEN** every direct importer of `Counter` is recomputed

#### Scenario: Reuse across a body edit

- **WHEN** a dependency edits the body of `impl Counter { pub fn value ... }` without changing its contract
- **THEN** the surface is equal and importers are reused

#### Scenario: Round-trip an encoded member

- **WHEN** a surface carrying a generic receiver method is encoded and decoded
- **THEN** the decoded member is equal to the original including owner-then-local binder order
