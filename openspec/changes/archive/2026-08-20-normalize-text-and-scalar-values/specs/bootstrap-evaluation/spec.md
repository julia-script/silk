## MODIFIED Requirements

### Requirement: Evaluation preserves first-class string semantics

Evaluation SHALL model `string` as valid immutable text with storage provenance, byte length, and
lexical lifetime distinct from a byte slice. It SHALL agree with emitted targets on ordinary
references to string values, explicit byte viewing, `char` traversal, exact equality, safe
validation results, owned-string views, checked scalar conversion, and loan endings without using
host-string identity as observable semantics.

#### Scenario: Compare exact strings

- **WHEN** evaluation compares equal UTF-8 sequences and then compares canonically equivalent but byte-distinct sequences
- **THEN** it reports equality only for the exact sequence pair without normalization

#### Scenario: Validate a runtime byte view

- **WHEN** stdlib validation receives valid and invalid runtime byte views
- **THEN** evaluation returns the borrowing `string` for the valid input and the typed invalid-UTF-8 value for the invalid input

#### Scenario: Traverse a non-ASCII scalar

- **WHEN** evaluation traverses a valid multi-byte UTF-8 sequence
- **THEN** it produces the exact `char` and next byte offset through checked scalar conversion

#### Scenario: Reject an invalid scalar conversion

- **WHEN** evaluation checks a surrogate or a value above `0x10ffff`
- **THEN** it returns `None` without a trap or truncated character

#### Scenario: Keep invalid unchecked construction outside safe guarantees

- **WHEN** unsafe source forms `string` from malformed UTF-8
- **THEN** the program has violated the unsafe operation contract and evaluation does not publish a recoverable validation result
