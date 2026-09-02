## ADDED Requirements

### Requirement: Inherent members document under their owner

Documentation ownership SHALL treat a `///` block above a member of an inherent impl as that
member's documentation and a block above the impl head as attached to the head. Generated module
references SHALL present inherent members grouped under their owner type, labeled as methods or
associated functions, with the canonical `Owner.member` qualifier rather than a module-qualified
spelling, and intra-document links `[`member`]` inside the owner's module SHALL resolve to the
member.

#### Scenario: Present Option members under Option

- **WHEN** documentation is generated for a module declaring `pub union Option<T>` and `impl<T> Option<T> { pub fn map ... }`
- **THEN** the reference lists `map` under `Option` as a method spelled `Option.map`

#### Scenario: Resolve a member link

- **WHEN** the module documentation for `Option` links `[`map`]`
- **THEN** the link resolves to the inherent member rather than reporting an unresolved symbol
