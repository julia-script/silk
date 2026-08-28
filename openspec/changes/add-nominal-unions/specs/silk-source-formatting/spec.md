## ADDED Requirements

### Requirement: Nominal union declarations have one canonical layout

The formatter SHALL preserve comments and source meaning while rendering optional visibility,
`union`, the union name and type parameters, braces, source-ordered unit and named-field variants,
field visibility and types, separators, constructors, and patterns canonically. Multiline variants
and fields SHALL use deterministic indentation and trailing separators, and formatting SHALL remain
idempotent without changing variant or field identity.

#### Scenario: Format a generic mixed union

- **WHEN** a complete union contains unit and named-field variants with irregular whitespace
- **THEN** formatting emits one canonical generic declaration with stable variant and field indentation and preserves all comments

#### Scenario: Format an applied variant path

- **WHEN** construction or a pattern spells `Result<A, E>.Success { value }`
- **THEN** formatting preserves the applied parent before the dot and formats the field body under the ordinary struct-like policy

