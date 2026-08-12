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

### Requirement: Semantic text boundaries use string

Shipped standard-library APIs SHALL use `string` for complete logging messages, normalized path
construction and resolution, path text accessors, and native filesystem roots. Implementations
SHALL request UTF-8 byte views explicitly where text reaches byte storage, standard streams, or raw
OS operations. APIs whose domain is arbitrary bytes, including `Bytes`, whole-file contents, and
standard streams, SHALL remain byte-oriented.

#### Scenario: Log semantic text

- **WHEN** source submits a complete message through `Effect.log`, `Effect.logAt`, or `Logger.log`
- **THEN** the API accepts `string` and a provider converts it to bytes only if its output boundary requires an encoding

#### Scenario: Construct and inspect paths as text

- **WHEN** source constructs, joins, resolves, or inspects a normalized `Path`
- **THEN** the textual inputs and borrowed textual outputs use `string` without exposing the path's owned byte storage

#### Scenario: Preserve binary boundaries

- **WHEN** source writes arbitrary file contents or standard-stream bytes, or a provider invokes a raw OS intrinsic
- **THEN** that boundary continues to use byte views and any textual caller performs an explicit UTF-8 conversion
