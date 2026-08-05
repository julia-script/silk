## MODIFIED Requirements

### Requirement: First bootstrap entry point

Bootstrap evaluation SHALL consume the elaboration result and SHALL select a top-level function
named `main` only when lookup resolves to exactly one declaration, that declaration has zero
parameters, and its declared return type is the resolved bootstrap `I32` type. Evaluation SHALL
return a closed `Blocked` outcome rather than throw or fail for a missing, ambiguous,
parameterized, damaged, or incorrectly typed entry declaration. Every blocked entry outcome SHALL
retain the available lookup, declaration, and syntax provenance.

#### Scenario: Select one valid main

- **WHEN** the elaboration result contains exactly one zero-parameter `main` declaring `I32`
- **THEN** evaluation selects that exact declaration as the entry point

#### Scenario: Block a missing main

- **WHEN** no top-level declaration is named `main`
- **THEN** evaluation is blocked with a missing-entry reason and does not select another function

#### Scenario: Block ambiguous main declarations

- **WHEN** multiple top-level declarations are named `main`
- **THEN** evaluation is blocked with every matching declaration identity and does not choose the first

#### Scenario: Block a parameterized main

- **WHEN** the unique `main` declaration has one or more parameters
- **THEN** evaluation is blocked with the exact entry identity and actual parameter count
