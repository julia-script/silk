## ADDED Requirements

### Requirement: Shipped source provides owned String and UTF-8 policy

Canonical shipped Silk source SHALL define nominal `String`, a typed `InvalidUtf8` result member,
complete UTF-8 validation, effectful copying from `string`, allocation-free `String` viewing, and
explicit byte-length, UTF-8 byte-view, and Unicode-scalar traversal functions. The owner SHALL use
ordinary allocation and collection source, preserve valid UTF-8 after every safe operation, and
remain navigable and diagnosable like user source. No safe function SHALL publish a partial string,
hide allocation, or return a view that outlives its backing storage.

#### Scenario: Navigate to String behavior

- **WHEN** editor tooling resolves a call that copies or views a `String`
- **THEN** go-to-definition opens the canonical shipped Silk implementation rather than a compiler-generated declaration

#### Scenario: Preserve UTF-8 across mutation

- **WHEN** safe stdlib operations build or extend owned `String` from valid `string` inputs
- **THEN** every subsequently borrowed view remains valid UTF-8 with the exact concatenated scalar sequence

#### Scenario: Report allocation failure honestly

- **WHEN** copying a non-empty `string` cannot obtain owned storage
- **THEN** the existing typed allocation failure is returned and no incomplete `String` escapes

### Requirement: Unicode policy remains explicit stdlib behavior

Scalar decoding, normalization, grapheme segmentation, case mapping, and locale-sensitive
comparison SHALL be ordinary, explicitly invoked stdlib behavior. The initial string surface MUST
NOT claim implicit normalization or a generic character unit, and later Unicode data versions MUST
be independently testable without changing compiler type identity or target ABI.

#### Scenario: Request scalar traversal

- **WHEN** source traverses a `string` containing one-byte and multi-byte scalars
- **THEN** the stdlib yields the exact Unicode scalar sequence without exposing continuation bytes as characters

#### Scenario: Normalize explicitly

- **WHEN** source requests a named normalization form for canonically equivalent strings
- **THEN** normalization follows the stdlib's declared Unicode policy rather than changing ordinary equality
