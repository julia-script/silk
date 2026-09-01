## Purpose

Provide an immutable, byte-exact source representation and owner-qualified spans that every later
compiler phase can reference without assuming the source is valid text.

## ADDED Requirements

### Requirement: Exact immutable source bytes

A source file SHALL own an immutable snapshot of the exact bytes supplied by its caller, including
invalid UTF-8, zero bytes, and line-ending spelling. Later mutation of the caller's input buffer
MUST NOT change the source file.

#### Scenario: Preserve arbitrary bytes

- **WHEN** a caller creates a source file from bytes containing invalid UTF-8 and mixed line endings
- **THEN** reading the source file returns the same byte sequence without replacement or normalization

#### Scenario: Isolate the source snapshot

- **WHEN** the caller mutates its original byte buffer after creating a source file
- **THEN** the source file's bytes remain unchanged

### Requirement: Explicit logical source identity

Every source file SHALL carry an explicit, copyable logical identity supplied by the caller. Source
identity MUST NOT be inferred from a current directory, filesystem traversal, object identity, or
the contents of the file.

#### Scenario: Equal bytes with different identities

- **WHEN** two source files contain equal bytes but have different caller-supplied identities
- **THEN** they remain distinct source files for span ownership and diagnostics

### Requirement: Owner-qualified half-open spans

A source span SHALL identify exactly one source file and a half-open byte range `[start, end)` whose
offsets satisfy `0 <= start <= end <= source length`. Empty spans at any valid byte boundary,
including end-of-file, SHALL be representable.

#### Scenario: Span a token

- **WHEN** a token occupies bytes 4 through 9 of a source file
- **THEN** its span has start 4, end 9, and the identity of that source file

#### Scenario: Represent end-of-file

- **WHEN** a source file contains `N` bytes
- **THEN** the end-of-file span is the empty range `[N, N)` owned by that source file

### Requirement: Exact span slicing

Slicing a source file with one of its valid spans SHALL return exactly the bytes in that range.
Applying a span owned by another source file MUST be rejected rather than reading coincidentally
equal offsets.

#### Scenario: Slice a source token

- **WHEN** a valid span covers the bytes spelling `return`
- **THEN** slicing the owning source file returns exactly those six bytes

#### Scenario: Reject a foreign span

- **WHEN** a span from one source file is applied to another source file
- **THEN** the operation rejects the ownership mismatch without returning bytes
