## ADDED Requirements

### Requirement: Bool is a built-in declared type

Declared parameter and return types SHALL resolve `Bool` as a built-in semantic type alongside
`I32`. Any other spelling SHALL keep the existing `SEM0001` unknown-type diagnostic.

#### Scenario: Resolve a Bool parameter and return

- **WHEN** `pub fn negate(flag: Bool) -> Bool { return flag }` is collected
- **THEN** the parameter and return types resolve to `Bool` with no diagnostics

#### Scenario: Keep unknown types diagnosed

- **WHEN** a return type spells `Boolean`
- **THEN** the type stays unresolved with one `SEM0001` diagnostic
