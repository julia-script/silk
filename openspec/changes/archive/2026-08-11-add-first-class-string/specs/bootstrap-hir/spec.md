## ADDED Requirements

### Requirement: HIR preserves first-class string identity

HIR SHALL represent text literals and all subsequently typed string expressions with canonical
`string` type identity, exact storage provenance, and any lexical loan required by borrowed backing
storage. It MUST NOT encode `string` as a shared `u8` slice or infer byte indexing from its physical
representation.

#### Scenario: Carry a borrowed owned-string view

- **WHEN** elaboration accepts a stdlib view operation over a shared `String` borrow
- **THEN** HIR records a `string` result tied to that borrow rather than an unrelated slice or owner

#### Scenario: Keep literal and runtime views type-identical

- **WHEN** one function accepts both a static text literal and a validated runtime UTF-8 view
- **THEN** HIR gives both arguments canonical `string` type while retaining their distinct storage provenance
